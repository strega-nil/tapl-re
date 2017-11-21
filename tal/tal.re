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


let main = () => {
  let nats = {|
  type id_t = unit -> unit in
  type nat = id_t -> unit -> unit in
  let n = (
    let succ =
      /n:nat./s:id_t./z:unit.s (n s z)
    in
    let zero =
      /s:id_t./z:unit.z
    in
    succ (succ (succ zero))
  ) in n @ ()
  |};

  let print_tm = true;
  let print_eval = true;

  let term =
    switch (Parse.parse(nats)) {
    | Result.Ok(term) => term
    | Result.Err(e) =>
      print_string("error: ");
      Parse.print_parser_error(e) |> print_newline;
      exit(1)
    };

  if (print_tm) {
    Lambda.print_term(term) |> print_newline
  };

  switch (finish(term)) {
  | Result.Ok(ast) =>
    if (print_eval) {
      eval_and_print(ast) |> ignore
    } else {
      eval(ast) |> ignore
    }
  | Result.Err(e) =>
    print_string("error: ");
    Lambda.print_type_error(e) |> print_newline;
    exit(2)
  };
};

main();
