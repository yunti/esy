module Packument = struct
  type t = {
    versions : Json.t StringMap.t;
    distTags : SemverVersion.Version.t StringMap.t [@key "dist-tags"];
  } [@@deriving of_yojson { strict = false }]
end

module MetadataCache = struct

  let cachePath = Path.(currentPath () / "esy-npm-metadata-cache")
  let dbPath = Path.(cachePath / "db.json")

  type t = string StringMap.t [@@deriving yojson]

  let load () =
    let open RunAsync.Syntax in
    match%lwt
      let%bind json = Fs.readJsonFile dbPath in
      let%bind db = RunAsync.ofRun (Json.parseJsonWith of_yojson json) in
      return db
    with
    | Ok db -> return db
    | Error _ -> return StringMap.empty

  let save db =
    let json = to_yojson db in
    Fs.writeJsonFile ~json dbPath

  let get name db =
    StringMap.find_opt name db

  let set name etag db =
    StringMap.add name etag db

  let write name etag data _db =
    Fs.writeFile ~data Path.(cachePath / (name ^ "-" ^ etag))

  let read name db =
    match StringMap.find_opt name db with
    | Some etag ->
      Fs.readFile Path.(cachePath / (name ^ "-" ^ etag))
    | None ->
      RunAsync.error "no cached data found"

end

type versions = {
  versions : SemverVersion.Version.t list;
  distTags : SemverVersion.Version.t StringMap.t;
}

module VersionsCache = Memoize.Make(struct
  type key = string
  type value = versions option RunAsync.t
end)

module PackageCache = Memoize.Make(struct
  type key = string * SemverVersion.Version.t
  type value = Package.t RunAsync.t
end)

type t = {
  url : string;
  versionsCache : VersionsCache.t;
  pkgCache : PackageCache.t;
  queue : LwtTaskQueue.t;
  mutable metadataCache : MetadataCache.t;
}

let () = Curl.(global_init CURLINIT_GLOBALALL)

let rec retryInCaseOfError ~num ~desc  f =
  match%lwt f () with
  | Ok resp -> RunAsync.return resp
  | Error _ when num > 0 ->
    let%lwt () =
      Logs_lwt.warn (fun m -> m "failed to %s, retrying (attempts left: %i)" desc num)
    in
    retryInCaseOfError ~num:(num - 1) ~desc f
  | Error err -> Lwt.return (Error err)

let make ?(concurrency=80) ?url () =
  let open RunAsync.Syntax in
  let url =
    match url with
    | None -> "http://registry.npmjs.org/"
    | Some url -> url
  in
  let%bind metadataCache = MetadataCache.load () in
  return {
    url;
    versionsCache = VersionsCache.make ();
    pkgCache = PackageCache.make ();
    queue = LwtTaskQueue.create ~concurrency ();
    metadataCache;
  }

let findEtag =
  Re.(compile (seq [str "ETag: \""; group (rep1 alnum); str "\""]))

let versions ?(fullMetadata=false) ~name registry () =
  let open RunAsync.Syntax in
  let fetchVersions () =
    let fetch =
      let name = Str.global_replace (Str.regexp "/") "%2f" name in
      let desc = Format.asprintf "fetch %s" name in
      retryInCaseOfError ~num:3 ~desc
        (fun () ->

          let connection = Curl.init () in
          Curl.set_url connection (registry.url ^ "/" ^ name);

          Curl.set_httpheader connection ["Accept-Encoding: gzip"];
          if not fullMetadata then
            Curl.set_httpheader connection ["Accept: application/vnd.npm.install-v1+json"];

          begin match MetadataCache.get name registry.metadataCache with
          | Some etag ->
            let header = Printf.sprintf {|If-None-Match: "%s"|} etag in
            Curl.set_httpheader connection [header];
            Lwt.return ()
          | None ->
            Lwt.return ()
          end;%lwt

          Curl.set_sslverifypeer connection true;
          Curl.set_sslverifyhost connection Curl.SSLVERIFYHOST_HOSTNAME;

          let result = Buffer.create 16384 in
          let nextEtag = ref None in

          let onWrite chunk =
            Buffer.add_string result chunk;
            String.length chunk
          in

          let onHeader header =
            begin match Re.exec_opt findEtag header with
            | Some res ->
              let etag = Re.Group.get res 1 in
              nextEtag := Some etag
            | None -> ()
            end;
            String.length header
          in

          Curl.set_writefunction connection onWrite;
          Curl.set_headerfunction connection onHeader;

          Lwt.finalize (fun () ->
            match%lwt Curl_lwt.perform connection with
            | Curl.CURLE_OK ->
              begin match Curl.get_responsecode connection with
              | 200 ->
                let data = Buffer.contents result in
                let%bind () =
                  match !nextEtag with
                  | Some etag ->
                    registry.metadataCache <- MetadataCache.set name etag registry.metadataCache;
                    MetadataCache.write name etag data registry.metadataCache
                  | None -> return ()
                in
                let%bind () = MetadataCache.save registry.metadataCache in
                return (CurlWrapper.Success data)
              | 304 ->
                let%bind data = MetadataCache.read name registry.metadataCache in
                return (CurlWrapper.Success data)
              | responseCode -> errorf "response code: %i" responseCode
              end
            | _ -> error "oops")
            (fun () -> Curl.cleanup connection; Lwt.return ())
        )
    in
    match%bind fetch with
    | CurlWrapper.NotFound -> return None
    | CurlWrapper.Success data ->
      let%bind packument =
        RunAsync.context (
        Json.parseStringWith Packument.of_yojson data
        |> RunAsync.ofRun
        ) "parsing packument"
      in
      let%bind versions = RunAsync.ofStringError (
        let f (version, packageJson) =
          let open Result.Syntax in
          let%bind version = SemverVersion.Version.parse version in
          PackageCache.ensureComputed registry.pkgCache (name, version) begin fun () ->
            let version = Version.Npm version in
            RunAsync.ofRun (PackageJson.packageOfJson ~name ~version packageJson)
          end;
          return version
        in
        packument.Packument.versions
        |> StringMap.bindings
        |> Result.List.map ~f
      ) in
      return (Some {versions; distTags = packument.Packument.distTags;})
  in
  fetchVersions
  |> LwtTaskQueue.queued registry.queue
  |> VersionsCache.compute registry.versionsCache name

let package ~name ~version registry () =
  let open RunAsync.Syntax in
  let%bind _: versions option = versions ~fullMetadata:true ~name registry () in
  match PackageCache.get registry.pkgCache (name, version) with
  | None -> errorf "no package found on npm %s@%a" name SemverVersion.Version.pp version
  | Some pkg -> pkg
