module type Lambda_impl_type = {
  type term;
  type ast;

  let var: string => term;
  let abs: string => term => term;
  let app: term => term => term;

  let finish: term => ast;

  /* if eval1 is finished, it returns None */
  let eval1: ast => option(ast);
  let print_ast: ast => unit;
  let print_term: term => unit;
};
module type Lambda_helper_type = (M: Lambda_impl_type) => {
  let (/>): M.term => M.term => M.term;
  let (@>): string => M.term => M.term;
  let var: string => M.term;

  let print_term: M.term => unit;
  let print_ast: M.ast => unit;

  let finish: M.term => M.ast;

  let eval1: M.ast => M.ast;
  let eval: M.ast => M.ast;
  let eval_and_print: M.ast => M.ast;
};
module Lambda_helper: Lambda_helper_type = (M: Lambda_impl_type) => {
  let (/>) = M.app;
  let (@>) = M.abs;
  let var = M.var;

  let print_term = M.print_term;
  let print_ast = M.print_ast;

  let finish = M.finish;

  let eval1 = (ast) => {
    switch (M.eval1(ast)) {
    | Some(ast) => ast
    | None => ast
    }
  };
  let rec eval = (ast) => {
    switch (M.eval1(ast)) {
    | Some(ast) => eval(ast)
    | None => ast
    }
  };
  let rec eval_and_print = (ast) => {
    print_ast(ast) |> print_newline;
    switch (M.eval1(ast)) {
    | Some(ast) => eval_and_print(ast)
    | None => ast
    }
  };
};
