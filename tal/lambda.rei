type term;

type ty;

type ast;

type type_error;

let ty_unit: unit => ty;

let ty_lam: (ty, ty) => ty;

let unit: unit => term;

let marker: unit => term;

let var: string => term;

let abs: (ty, string, term) => term;

let app: (term, term) => term;

let finish: term => Result.t(ast, type_error);

let typeof: ast => ty; /* if eval1 is finished, it returns None */

let eval1: ast => option(ast);

let print_ty: ty => unit;

let print_term: term => unit;

let print_ast: ast => unit;

let print_type_error: type_error => unit;
