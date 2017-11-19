let rec eval = (ast) => {
  switch (Lambda.eval1(ast)) {
  | Some(ast) => eval(ast)
  | None => ast
  }
};

let rec eval_and_print = (ast) => {
  Lambda.print_ast(ast) |> print_newline;
  switch (Lambda.eval1(ast)) {
  | Some(ast) => eval_and_print(ast)
  | None => ast
  }
};

let finish = Lambda.finish;

let term = Parse.parse("/x:unit.x x");
Lambda.print_term(term) |> print_newline;
eval_and_print(finish(term));
