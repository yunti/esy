%token <string> ID
%token AT
%token SLASH
%token PLUS
%token LPAREN
%token RPAREN
%token EOF

%left PLUS

%{

%}

%start start
%type <DepSpecImpl.t> start

%%

start:
  e = expr; EOF { e }

expr:
    e = select { e }
  | e = package { e }
  | e = union { e }

id:
    id = ID; {
      match id with
      | "root" -> DepSpecImpl.root
      | "self" -> DepSpecImpl.self
      | name -> DepSpecImpl.name name
    }
  | AT; scope = ID; SLASH; name = ID; {
      let name = "@" ^ scope ^ "/" ^ name in
      DepSpecImpl.name name
    }

package:
  id = id; { DepSpecImpl.package id }

select:
  select = ID; LPAREN; id = id; RPAREN {
    match select with
    | "dependencies" -> DepSpecImpl.dependencies id
    | "devDependencies" -> DepSpecImpl.devDependencies id
    | _ -> $syntaxerror
  }

union:
  a = expr; PLUS; b = expr {
    DepSpecImpl.(a + b)
  }

%%


