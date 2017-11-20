type ty =
| Ty_unit
| Ty_lam(ty, ty);

type term =
| Term_marker
| Term_unit
| Term_var(string)
| Term_abs(ty, string, term)
| Term_app(term, term);

type ast =
| Ast_marker
| Ast_unit
| Ast_var(int)
| Ast_abs(ty, string, ast)
| Ast_app(ast, ast);

let ty_unit = () => Ty_unit;
let ty_lam = (lhs, rhs) => Ty_lam(lhs, rhs);

let unit = () => Term_unit;
let marker = () => Term_marker;

let var = (name) => Term_var(name);
let abs = (ty, name, body) => Term_abs(ty, name, body);
let app = (callee, parm) => Term_app(callee, parm);

let rec string_of_ty = (ty) => {
  let is_unit = (ty) => {
    switch (ty) {
    | Ty_unit => true
    | Ty_lam(_, _) => false
    }
  };

  switch (ty) {
  | Ty_unit => "unit"
  | Ty_lam(lhs, rhs) =>
    let lhs' = if (is_unit(lhs)) {
      "unit -> "
    } else {
      "(" ++ string_of_ty(lhs) ++ ") -> "
    };
    lhs' ++ string_of_ty(rhs)
  }
};

let print_ty = (ty) => print_string(string_of_ty(ty));

let rec print_term = (term) => {
  let is_cmplx = fun
  | Term_marker => false
  | Term_unit => false
  | Term_var(_) => false
  | _ => true;

  switch(term) {
  | Term_marker => print_char('@')
  | Term_unit => print_string("()")
  | Term_var(name) => print_string(name)
  | Term_abs(ty, var, body) =>
    print_char('/');
    print_string(var);
    print_char(':');
    print_ty(ty);
    print_char('.');
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

let string_of_ast = (ast) => {
  let rec bound_var_name = (idx, names) => switch (names) {
  | [name, ...names] =>
    if (idx == 0) { name } else { bound_var_name(idx - 1, names) }
  | [] => failwith("malformed lambda calculus ast")
  };

  let is_cmplx = (ast) => switch (ast) {
  | Ast_marker => false
  | Ast_unit => false
  | Ast_var(_) => false
  | _ => true
  };

  let rec soa_rec = (ast, names) => {
    let soa_cmplx = (ast) => {
      if (is_cmplx(ast)) {
        "(" ++ soa_rec(ast, names) ++ ")"
      } else {
        soa_rec(ast, names)
      }
    };
    switch (ast) {
    | Ast_marker => "@"
    | Ast_unit => "()"
    | Ast_var(idx) => bound_var_name(idx, names)
    | Ast_app(callee, parm) =>
      soa_cmplx(callee) ++ " " ++ soa_cmplx(parm)
    | Ast_abs(ty, name, body) =>
      "/" ++ name ++ ":" ++ string_of_ty(ty) ++ "." ++ soa_rec(body, [name, ...names])
    }
  };

  soa_rec(ast, [])
};

let print_ast = (ast) => print_string(string_of_ast(ast));

type type_error =
| Type_error_variable_not_found(string)
| Type_error_incorrect_types(ty, ty)
| Type_error_calling_non_callable(ty, ast);

let print_type_error = (err) => switch (err) {
| Type_error_variable_not_found(var) =>
  print_string("variable not found: " ++ var)
| Type_error_incorrect_types(lhs, rhs) =>
  print_string(
    "type mismatch: " ++ string_of_ty(lhs) ++ " != " ++ string_of_ty(rhs)
  )
| Type_error_calling_non_callable(ty, ast) =>
  print_string(
    "attempt to call non-callable: " ++ string_of_ty(ty)
    ++ "\n  " ++ string_of_ast(ast)
  )
};

let rec typeof_rec = (ast, tys) => switch (ast) {
| Ast_app(callee, _) as ast' =>
  switch (typeof_rec(callee, tys)) {
  | Result.Ok(Ty_lam(_, ret)) => Result.Ok(ret)
  | Result.Ok(ty) => Result.Err(Type_error_calling_non_callable(ty, ast'))
  | Result.Err(e) => Result.Err(e)
  }
| Ast_abs(ty, _, body) => 
  typeof_rec(body, [ty, ...tys]) |> Result.map((it) => Ty_lam(ty, it))
| Ast_unit => Result.Ok(Ty_unit)
| Ast_marker => Result.Ok(Ty_lam(Ty_unit, Ty_unit))
| Ast_var(idx) => Result.Ok(List.nth(tys, idx))
};

let typeof = (ast) => switch (typeof_rec(ast, [])) {
| Result.Ok(ty) => ty
| Result.Err(_) => failwith("malformed ast")
};

let finish = (tm) => {
  let rec get_var = (name, names, idx) => switch (names) {
  | [x, ..._] when x == name => Result.Ok(Ast_var(idx))
  | [_, ...xs] => get_var(name, xs, idx + 1)
  | [] => Result.Err(Type_error_variable_not_found(name))
  };
  let rec finish_rec = (tm, names, tys) => {
    switch (tm) {
    | Term_marker => Result.Ok(Ast_marker)
    | Term_unit => Result.Ok(Ast_unit)
    | Term_var(name) => get_var(name, names, 0)
    | Term_app(callee, parm) =>
      finish_rec(callee, names, tys) |> Result.bind((callee') =>
      finish_rec(parm, names, tys) |> Result.bind((parm') =>
      typeof_rec(callee', tys) |> Result.bind((it) =>
      switch (it) {
      | Ty_lam(parm_ty, _) =>
        typeof_rec(parm', tys) |> Result.bind((parm_ty') =>
        if (parm_ty != parm_ty') {
          Result.Err(Type_error_incorrect_types(parm_ty, parm_ty'));
        } else {
          Result.Ok(Ast_app(callee', parm'))
        })
      | ty => Result.Err(Type_error_calling_non_callable(ty, Ast_app(callee', parm')))
      })))
    | Term_abs(ty, name, body) =>
      finish_rec(body, [name, ...names], [ty, ...tys]) |> Result.map((body') =>
      Ast_abs(ty, name, body'))
    }
  };
  finish_rec(tm, [], [])
};

let substitute = (body, parm) => {
  let rec sub_rec = (body, parm, idx) => {
    switch (body) {
    | Ast_var(idx') when idx == idx' => parm
    | Ast_app(callee', parm') =>
      Ast_app(sub_rec(callee', parm, idx), sub_rec(parm', parm, idx))
    | Ast_abs(ty, name, body') =>
      Ast_abs(ty, name, sub_rec(body', parm, idx + 1))
    | unchanged => unchanged
    }
  };
  sub_rec(body, parm, 0)
};

let rec eval1 = (ast) => {
  let rec eval_app = (callee, parm) => {
    switch (callee) {
    | Ast_unit => None
    | Ast_var(_) => failwith("malformed lambda ast")
    | Ast_abs(_, _, body) => Some(substitute(body, parm))
    | Ast_marker =>
      switch (eval1(parm)) {
      | Some(parm') => Some(Ast_app(Ast_marker, parm'))
      | None => None
      }
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
  | Ast_marker => None
  | Ast_unit => None
  | Ast_var(_) => failwith("malformed lambda ast")
  | Ast_abs(_, _, _) => None
  | Ast_app(callee, parm) => eval_app(callee, parm)
  }
};
