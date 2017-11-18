type term;
type ast;

let var: string => term;
let abs: string => term => term;
let app: term => term => term;

let marker: unit => term;
let unit: unit => term;

let finish: term => ast;

/* if eval1 is finished, it returns None */
let eval1: ast => option(ast);
let print_ast: ast => unit;
let print_term: term => unit;
