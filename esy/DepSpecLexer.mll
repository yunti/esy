{
exception Error of string
}

let id          = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let space       = [ ' ' '\t' ]+

rule read = parse
 | space        { read lexbuf }
 | id as id     { DepSpecParser.ID id }
 | '('          { DepSpecParser.LPAREN }
 | ')'          { DepSpecParser.RPAREN }
 | '+'          { DepSpecParser.PLUS }
 | '-'          { DepSpecParser.MINUS }
 | '''          { literal (Buffer.create 16) lexbuf }
 | eof          { DepSpecParser.EOF }
 | _ as c       { raise (Error (Printf.sprintf "unexpected char: %c" c)) }

and literal buf = parse
 | '''             {
     STRING (Buffer.contents buf)
   }
 | '\\' '''        {
     Buffer.add_string buf "'";
     literal buf lexbuf
   }
 | [^ ''' '\\' ]+  {
     Buffer.add_string buf (Lexing.lexeme lexbuf);
     literal buf lexbuf
   }
 | _ as c          {
     let msg = Printf.sprintf "unexpected token: %c" c in
     raise (Error msg)
   }
 | eof         {
     let msg = Printf.sprintf "unexpected end of string" in
     raise (Error msg)
   }
