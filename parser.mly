
/* Analyseur syntaxique pour mini-Turtle */

%{
  open Ast

let rec handle_elif bel e =
match bel with
| (b0, e0)::[] -> Bif (b0, e0, e)
| (b0, e0)::s -> Bif (b0, e0, 
    {desc = Ebexpr (handle_elif s e); loc = e.loc})
| [] -> failwith "Impossible case"
%}

/* Déclaration des tokens */

%token <Ast.atom> ATM
%token <Ast.binop> CMP
%token LT GT
%token <string> IDENT
%token IF ELSE ELIF RETURN FUN FN THEN VAL VAR
%token EOF
%token LP RP LSQ RSQ LBRACE RBRACE COMMA DOT COLON SCOLON 
%token EQUAL ASSIGN NEGATE DEREF ARROW
%token CONCAT AND OR
%token PLUS MINUS TIMES DIV MOD


/* Priorités et associativités des tokens */

%nonassoc IF
%left OR
%left AND
%nonassoc CMP ASSIGN LT GT
%left PLUS MINUS CONCAT
%left TIMES DIV MOD
%nonassoc DEREF NEGATE
//%nonassoc DOT 
//%nonassoc THEN
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file
%type <Ast.decl> decl
%type <Ast.funbody> funbody
%type <Ast.param> param
%type <Ast.annot> annot
%type <Ast.result> result
%type <Ast.kokatype> kokatype
%type <Ast.akokatype> akokatype
%type <Ast.atom> atom
%type <Ast.expr> expr
%type <Ast.desc> desc
%type <Ast.bexpr> bexpr
%type <Ast.block> block
%type <Ast.stmt> stmt
%type <Ast.ident> ident



%%

/* Règles de grammaire */

file:
  SCOLON* dl = separated_list(SCOLON+, decl) EOF
    { dl }
;

decl:
  FUN id = ident fb = funbody
    {id, fb}
;

funbody:
  LP pl = separated_list(COMMA, param) RP a = annot e = expr
    {pl, a, e}
;

param:
  id = ident COLON k = kokatype
    {id, k}
;

annot:
| COLON r = result {Annot r}
| {Noannot}
;

result:
| k = kokatype {[], k}
| LT idl = separated_list(COMMA, ident) GT k = kokatype
    {idl, k}
;

kokatype:
| a = akokatype
    {Katype a}
| a = akokatype ARROW r = result
    {Kmonoparam (a, r)}
| LP kl = separated_list(COMMA, kokatype) RP ARROW r = result
    {Kpluriparam (kl, r)}
;

akokatype:
| id = ident 
    {Aknoparam id}
| id = ident LT k = kokatype GT
    {Akparam (id, k)}
| LP k = kokatype RP
    {Akbracket k}
| LP RP
    {Akunit}
;

atom:
| a = ATM
    {a}
| LP RP 
    {Aunit}
| id = ident
    {Aident id}
| LP e = expr RP
    {Aexpr e}
| a = atom LP el = separated_list(COMMA, expr) RP
    {Acall (a, el)}
| a = atom DOT id = ident
    {Acall (Aident id, [{desc = Ebexpr (Batom a); loc = $startpos, $endpos}])}
| a = atom FN fb = funbody
    {match a with
    | Acall (a', el) -> 
      Acall (a', el @ [{desc = Ebexpr (Bfn fb); loc = $startpos, $endpos}])
    | _ -> Acall (a, [{desc = Ebexpr (Bfn fb); loc = $startpos, $endpos}])}
| a = atom b = block
    {match a with
    | Acall (a', el) -> 
      Acall (a', el @ [{desc = Ebexpr (Bfn ([], Noannot, {desc = Eblock b;
      loc = $startpos, $endpos}));
      loc = $startpos, $endpos}])
    | _ -> 
      Acall (a, [{desc = Ebexpr (Bfn ([], Noannot, {desc = Eblock b;
      loc = $startpos, $endpos}));
      loc = $startpos, $endpos}])}
| LSQ el = separated_list(COMMA, expr) RSQ
    {Alist el}
;

expr:
  d = desc
    { {desc = d; loc = $startpos, $endpos} }
;

desc:
| b = block
    {Eblock b}
| b = bexpr
    {Ebexpr b}
;

bexpr:
| a = atom
    {Batom a}
| NEGATE b = bexpr
    {Bneg b}
| DEREF b = bexpr
    {Bderef b}
| b1 = bexpr b = binop b2 = bexpr
    {Bbinop (b, b1, b2)}
| id = ident ASSIGN b = bexpr
    {Bassign (id, b)}
| IF bel = separated_nonempty_list(ELIF, separated_pair(bexpr, THEN, expr)) ELSE e = expr
    {handle_elif bel e}
| IF bel = separated_nonempty_list(ELIF, separated_pair(bexpr, THEN, expr))
    {handle_elif bel {desc = Eblock []; loc = $startpos, $endpos}}
| IF b = bexpr RETURN e = expr
    {Bif (b, {desc = Ebexpr (Breturn e); loc = $startpos, $endpos}, 
        {desc = Eblock []; loc = $startpos, $endpos})}
| FN fb = funbody
    {Bfn fb}
| RETURN e = expr
    {Breturn e}
;

block:
  LBRACE SCOLON* sl = separated_list(SCOLON+, stmt) RBRACE
    {sl}
;

stmt:
| b = bexpr
    {Sbexpr b}
| VAL id = ident EQUAL e = expr
    {Sval (id, e)}
| VAR id = ident ASSIGN e = expr
    {Svar (id, e)}
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| LT    { Blt  }
| GT    { Bgt  }
| CONCAT{ Bconcat}
| AND   { Band }
| OR    { Bor  }
;

ident:
  id = IDENT
    { id }
;
