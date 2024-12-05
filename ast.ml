(* Arbres de syntaxe abstraite de Mini-Python *)

type ident = string

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Bconcat | Band | Bor                (* ++ && ||*)

type file = decl list

and decl = ident * funbody

and funbody = param list * annot * expr

and param = ident * kokatype

and annot = 
  | Noannot
  | Annot of result

and result = ident list * kokatype

and kokatype = 
  | Katype of akokatype
  | Kmonoparam of akokatype * result
  | Kpluriparam of kokatype list * result

and akokatype =
  | Aknoparam of ident
  | Akparam of ident * kokatype
  | Akbracket of kokatype
  | Akunit

and atom =
  | Aunit
  | Abool of bool
  | Astring of string
  | Aint of int
  | Aident of ident
  | Aexpr of expr
  | Acall of atom * expr list
  | Alist of expr list

and expr = {desc : desc; loc : Lexing.position * Lexing.position}

and desc = 
  | Eblock of block
  | Ebexpr of bexpr

and bexpr = 
  | Batom of atom
  | Bneg of bexpr
  | Bderef of bexpr
  | Bbinop of binop * bexpr * bexpr
  | Bassign of ident * bexpr
  | Bif of bexpr * expr * expr
  | Bfn of funbody
  | Breturn of expr

and block = stmt list

and stmt = 
  | Sbexpr of bexpr
  | Sval of ident * expr
  | Svar of ident * expr
