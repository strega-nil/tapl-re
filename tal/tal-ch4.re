module Term_types = {
  type numeric =
  | Numeric_zero
  | Numeric_succ(numeric);
  type value =
  | Value_true
  | Value_false
  | Value_numeric(numeric);

  type term =
  | Term_true
  | Term_false
  | Term_zero
  | Term_if(term, term, term)
  | Term_succ(term)
  | Term_pred(term)
  | Term_iszero(term);
  
  exception ExecutionIsStuck;

};

module type Term = {
  let evaluate: Term_types.term => Term_types.value;
};

module SmallStepTerm: Term = {
  open Term_types;

  let rec is_numeric = fun
  | Term_zero => true
  | Term_succ(n) => is_numeric(n)
  | _ => false;

  let is_value = fun
  | Term_true => true
  | Term_false => true
  | tm => is_numeric(tm);

  let rec eval1: term => option(term) = fun
  | Term_if(Term_true, then_, else_) => Some(then_)
  | Term_if(Term_false, then_, else_) => Some(else_)
  | Term_if(cond, then_, else_) =>
    switch (eval1(cond)) {
    | Some(cond') => Some(Term_if(cond', then_, else_))
    | None => raise(ExecutionIsStuck)
    }
  | Term_succ(t) =>
    switch(eval1(t)) {
    | Some(t') => Some(Term_succ(t'))
    | None => raise(ExecutionIsStuck)
    }
  | Term_pred(Term_zero) => Some(Term_zero)
  | Term_pred(Term_succ(nv)) when is_numeric(nv) => Some(nv)
  | Term_pred(t) =>
    switch(eval1(t)) {
    | Some(t') => Some(Term_pred(t'))
    | None => raise(ExecutionIsStuck)
    }
  | Term_iszero(Term_zero) => Some(Term_true)
  | Term_iszero(Term_succ(nv)) when is_numeric(nv) => Some(Term_false)
  | Term_iszero(t) =>
    switch (eval1(t)) {
    | Some(t') => Some(Term_iszero(t))
    | None => raise(ExecutionIsStuck)
    }
  | _ => None;

  let rec eval_number = fun
  | Term_zero => Numeric_zero
  | Term_succ(tm) => Numeric_succ(eval_number(tm))
  | _ => failwith("this shouldn't happen");

  let rec evaluate = (tm) => {
    switch (eval1(tm)) {
    | Some(tm') => evaluate(tm')
    | None =>
      switch (tm) {
      | Term_true => Value_true
      | Term_false => Value_false
      | Term_zero as num
      | Term_succ(_) as num => Value_numeric(eval_number(num))
      | _ => failwith("this shouldn't happen either")
      }
    }
  };
};

module BigStepTerm: Term = {
  open Term_types;

  let rec evaluate = fun
  | Term_true => Value_true
  | Term_false => Value_false
  | Term_zero => Value_numeric(Numeric_zero)
  | Term_succ(tm) =>
    switch (evaluate(tm)) {
    | Value_numeric(n) => Value_numeric(Numeric_succ(n))
    | _ => raise(ExecutionIsStuck)
    }
  | Term_if(cond, then_, else_) =>
    switch (evaluate(cond)) {
    | Value_true => evaluate(then_)
    | Value_false => evaluate(else_)
    | _ => raise(ExecutionIsStuck)
    }
  | Term_pred(tm) =>
    switch (evaluate(tm)) {
    | Value_numeric(Numeric_zero) as z => z
    | Value_numeric(Numeric_succ(n)) => Value_numeric(n)
    | _ => raise(ExecutionIsStuck)
    }
  | Term_iszero(tm) =>
    switch (evaluate(tm)) {
    | Value_numeric(Numeric_zero) => Value_true
    | Value_numeric(Numeric_succ(n)) => Value_false
    | _ => raise(ExecutionIsStuck)
    }
};

open Term_types;

let print_value = fun
| Value_true => print_string("true")
| Value_false => print_string("false")
| Value_numeric(n) => {
  let rec int_of_numeric = fun
  | Numeric_zero => 0
  | Numeric_succ(n) => 1 + int_of_numeric(n);
  print_int(int_of_numeric(n))
};

let evaluate = (m, tm) => {
  let module Tm = (val(m: (module Term)));
  Tm.evaluate(tm)
};

let main() = {
  let tif(c, t, e) = Term_if(c, t, e);
  let ast = tif(Term_true, Term_true, tif(Term_false, Term_false, Term_false));
  print_value(evaluate((module SmallStepTerm), ast)) |> print_newline;
  print_value(evaluate((module BigStepTerm), ast)) |> print_newline
};

main ();