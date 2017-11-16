
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

module Lambda_impl: Lambda_impl_type = {
  type term =
  | Term_var(string)
  | Term_abs(string, term)
  | Term_app(term, term);
  
  type ast =
  | Ast_free_var(string)
  | Ast_bound_var(int)
  | Ast_abs(string, ast)
  | Ast_app(ast, ast);

  let var = (name) => Term_var(name);
  let abs = (name, body) => Term_abs(name, body);
  let app = (callee, parm) => Term_app(callee, parm);


  let rec print_term = (term) => {
    let is_cmplx = fun
    | Term_var(_) => false
    | _ => true;

    switch(term) {
    | Term_var(name) => print_string(name)
    | Term_abs(var, body) =>
      print_string("\\" ++ var ++ ".");
      print_term(body);
    | Term_app(callee, parm) =>
      if (is_cmplx(callee)) { print_char('(') };
      print_term(callee);
      if (is_cmplx(callee)) { print_char(')') };
      print_char(' ');
      if (is_cmplx(parm)) { print_char('(') };
      print_term(parm);
      if (is_cmplx(parm)) { print_char(')') };
    }
  };

  let print_ast = (ast) => {
    let rec bound_var_name = (idx, names) => switch (names) {
    | [name, ...names] =>
      if (idx == 0) { name } else { bound_var_name(idx - 1, names) }
    | [] => failwith("malformed lambda calculus ast")
    };

    let is_cmplx = (ast) => switch (ast) {
    | Ast_free_var(_) | Ast_bound_var(_) => false
    | _ => true
    };
 
    let rec print_ast_rec = (ast, names) => {
      let print_cmplx = (ast) => {
        if (is_cmplx(ast)) {
          print_char('('); print_ast_rec(ast, names); print_char(')')
        } else {
          print_ast_rec(ast, names)
        }
      };
      switch (ast) {
      | Ast_free_var(name) => print_string(name);
      | Ast_bound_var(idx) => print_string(bound_var_name(idx, names))
      | Ast_app(callee, parm) =>
        print_cmplx(callee);
        print_char(' ');
        print_cmplx(parm)
      | Ast_abs(name, body) =>
        print_char('\\');
        print_string(name);
        print_char('.');
        print_ast_rec(body, [name, ...names])
      }
    };

    print_ast_rec(ast, [])
  };

  let finish = (tm) => {
    let rec get_var = (name, names, idx) => switch (names) {
    | [x, ..._] when x == name => Ast_bound_var(idx)
    | [_, ...xs] => get_var(name, xs, idx + 1)
    | [] => Ast_free_var(name)
    };
    let rec finish_rec = (tm, names) => {
      switch (tm) {
      | Term_var(name) => get_var(name, names, 0)
      | Term_app(callee, parm) =>
        Ast_app(finish_rec(callee, names), finish_rec(parm, names))
      | Term_abs(name, body) =>
        Ast_abs(name, finish_rec(body, [name, ...names]))
      }
    };
    finish_rec(tm, [])
  };

  let substitute = (body, parm) => {
    let rec sub_rec = (body, parm, idx) => {
      switch (body) {
      | Ast_free_var(_) as x => x
      | Ast_bound_var(idx') when idx == idx' => parm
      | Ast_bound_var(_) as x => x
      | Ast_app(callee', parm') =>
        Ast_app(sub_rec(callee', parm, idx), sub_rec(parm', parm, idx))
      | Ast_abs(name, body') =>
        Ast_abs(name, sub_rec(body', parm, idx + 1))
      }
    };
    sub_rec(body, parm, 0)
  };

  let rec eval1 = (ast) => {
    let rec eval_app = (callee, parm) => {
      switch (callee) {
      | Ast_free_var(_) => None
      | Ast_bound_var(_) => failwith("malformed lambda ast")
      | Ast_abs(_, body) => Some(substitute(body, parm))
      | Ast_app(callee', parm') =>
        switch (eval_app(callee', parm')) {
        | Some(callee) => Some(Ast_app(callee, parm))
        | None =>
          switch (eval1(parm)) {
          | Some(parm) => Some(Ast_app(callee, parm))
          | None => None
          }
        }
      }
    };

    switch (ast) {
    | Ast_free_var(_) => None
    | Ast_bound_var(_) => failwith("malformed lambda ast")
    | Ast_abs(_, _) => None
    | Ast_app(callee, parm) => eval_app(callee, parm)
    }
  };
};
module Lambda = Lambda_helper(Lambda_impl);

module Booleans = {
  open Lambda;
  let tru = "t" @> "f" @> var("t");
  let fls = "t" @> "f" @> var("f");

  let not_ = "b" @> (var("b") /> fls /> tru);

  let and_ = "b" @> "c" @> (var("b") /> var("c") /> fls);
  let or_ = "b" @> "c" @> (var("b") /> tru /> var("c"));
  let xor_ = "b" @> "c" @> (var("b") /> (not_ /> var("c")) /> var("c"));
};

module Pairs = {
  open Lambda;
  open Booleans;

  let pair = "f" @> "s" @> "b" @> (var("b") /> var("f") /> var("s"));
  let fst = "p" @> (var("p") /> tru);
  let snd = "p" @> (var("p") /> fls);
};

module Numbers = {
  open Lambda;
  open Booleans;
  open Pairs;

  let zero = "s" @> "z" @> var("z");
  let succ = "n" @> "s" @> "z" @>
    var("n") /> var("s") /> (var("s") /> var("z"));

  let plus = "n" @> "m" @>
    var("m") /> succ /> var("n");
  let times = "n" @> "m" @>
    var("m") /> (plus /> var("n")) /> zero;

  let iszero = {
    let make_fls = "n" @> fls;
    "n" @> var("n") /> make_fls /> tru;
  };

  let pred = {
    let zz = pair /> zero /> zero;
    let ss = "p" @>
      pair
      /> (snd /> var("p"))
      /> (plus /> (succ /> zero) /> (snd /> var("p")));
    "n" @> fst /> (var("n") /> ss /> zz);
  };

  let minus = "n" @> "m" @>
    var("m") /> pred /> var("n");

  let equal = "n" @> "m" @>
    and_
    /> (iszero /> (minus /> var("n") /> var("m")))
    /> (iszero /> (minus /> var("m") /> var("n")));
};

module Lists = {
  open Lambda;
  open Booleans;
  open Pairs;

  let nil = "c" @> "n" @>
    var("n");
  let cons = "h" @> "t" @> "c" @> "n" @>
    var("c") /> var("h") /> (var("t") /> var("c") /> var("n"));

  let isnil = {
    let ret_fls = "h" @> "t" @> fls;
    "l" @> var("l") /> ret_fls /> tru
  };

  let head = {
    let ret_hd = "h" @> "t" @> var("h");
    "l" @> var("l") /> ret_hd /> var("NIL");
  };

  let tail = {
    let ret_pair = "h" @> "t" @>
      pair
      /> (cons /> var("h") /> (fst /> var("t")))
      /> (fst /> var("t"));
    "l" @> snd /> (var("l") /> ret_pair /> (pair /> nil /> nil))
  };

  /** useful for printing */
  let app_list = "h" @> "t" @> var("h") /> var("t");
};

module Recursion = {
  open Lambda;

  let fix = {
    let rpt = "x" @>
      var("f") /> ("y" @> var("x") /> var("x") /> var("y"));
    "f" @> rpt /> rpt
  };
};

open Lambda;
open Numbers;
open Recursion;

let fct = {
  let one = succ /> zero;
  let g = "fct" @> "n" @>
    iszero /> var("n")
      /> one
      /> (times /> (var("fct") /> (pred /> var("n"))) /> var("n"));
  fix /> g
};

let res = fct /> (succ /> (succ /> (succ /> zero)));

let trm = res /> var("SUCC") /> var("ZERO");
let ast = finish(trm);
print_term(trm) |> print_newline;
ast |> eval_and_print;

/*
\.\.0
\.\.1 (1 0)
\.\.\.\.3 1 (2 1 0)
(\.(\.0)) (\.0)

G = a, b

  [b -> a] (b (\x.\y.b))
  [0 -> 1] 0 (\.\.2)
  1 (\.\.3)

  [b -> a (\z.a)] (b (\x.b))
  [0 -> 1 (\.2)] 0 (\.1)
  (1 (\.2)) (\.2)

  [b -> a] (\b.b a)
  [0 -> 1] (\.0 2)
  \.0 2

  [b -> a] (\a.b a)
  [0 -> 1] (\.1 0)
  \.2 0
*/
