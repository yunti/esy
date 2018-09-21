---
id: getting-started
title: Getting started
---

esy provides a single command called `esy`.

The typical workflow is to `cd` into a directory that contains a `package.json`
file, and then perform operations on that project.

## Typical Workflow

There are three main esy invocations which are used most frequently while
working on esy projects:

- `esy install`, installs dependencies of the project.

- `esy build`, builds dependencies of the project and the project itself.

- `esy <anycommand>`, executes `<anycommand>` within the sandbox dev
  environment.

Also there's `esy` command which is a shortcut for invoking `esy build` and then
`esy install`.

### Installing Esy

```bash
npm install -g esy
```

esy is meant to be installed globally via `npm`

### Clone the Project

```bash
git clone git@github.com:esy-ocaml/hello-reason.git
cd hello-reason
```

### Install Dependencies

```bash
% esy install
```

Installs the project's dependencies source code.

If `esy.lock.json` lockfile is found then it is used to fetch dependencies,
otherwise it is produced first by resolving version constraint in `package.json`.

### Build

```bash
% esy build
```

Ensures all dependencies are built and then builds the project.

This uses build store and thus not always performs actual builds of the
dependencies as they can already be built and cached.

### Test Compiled Executables

```bash
esy ./_build/default/bin/Hello.exe
```

Executes compiled executable inside esy development environment.

## Example Projects

There are example projects:

- [hello-reason](https://github.com/esy-ocaml/hello-reason), an example
  [Reason][] project which uses [dune][] build system.

- [hello-ocaml](https://github.com/esy-ocaml/hello-ocaml), an example [OCaml][]
  project which uses [dune][] build system.

## Other Useful Invocations

### Sandbox Shell

```bash
esy shell
```

Shells into the project's sandbox.

This is an alternative to running `esy <anycommand>` as `<anycommand>` can be
executed without `esy` prefix within the sandbox's shell.

### Getting Help on Esy Commands

```bash
esy help
```

See a list of esy commands and their description.

Help is automatically generated and thus always up to date.

[dune]: https://github.com/ocaml/dune
[reason]: https://reasonml.github.io/
[ocaml]: https://ocaml.org/
