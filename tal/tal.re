let rec eval = (ast) =>
  switch (Lambda.eval1(ast)) {
  | Some(ast) => eval(ast)
  | None => ast
  };

let rec eval_and_print = (ast) => {
  Lambda.print_ast(ast) |> print_newline;
  switch (Lambda.eval1(ast)) {
  | Some(ast) => eval_and_print(ast)
  | None => ast
  }
};

let finish = Lambda.finish;

let nats = {|
let n = (
  let succ =
    /n:(unit -> unit) -> unit -> unit.
      /s:unit -> unit./z:unit.
        s (n s z)
  in
  let zero =
    /s:unit -> unit./z:unit.z
  in
  succ (succ (succ zero))
) in n @ ()
|};

let term =
  switch (Parse.parse(nats)) {
  | Result.Ok(term) => term
  | Result.Err(e) =>
    Parse.print_parser_error(e) |> print_newline;
    exit(1)
  };

Lambda.print_term(term) |> print_newline;

switch (finish(term)) {
| Result.Ok(ast) => eval_and_print(ast) |> ignore
| Result.Err(e) =>
  print_string("error: ");
  Lambda.print_type_error(e) |> print_newline;
  exit(2)
};
