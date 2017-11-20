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
let nats = "
(/n:(unit -> unit) -> unit -> unit.
  n @ ()
) (
  (/succ:((unit -> unit) -> unit -> unit) -> ((unit -> unit) -> unit -> unit).
    (/zero: (unit -> unit) -> unit -> unit.
      succ (succ (succ zero))
    )
  )
  (/n:(unit -> unit) -> unit -> unit.
    /s:unit -> unit./z:unit.
      s (n s z)
  )
  (/s:unit -> unit./z:unit.z)
)";

let term = Parse.parse(nats);
Lambda.print_term(term) |> print_newline;
eval_and_print(finish(term));
