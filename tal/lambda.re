open Result.Monad;

type ty =
  | Ty_unit
  | Ty_lam(ty, ty)
  | Ty_named(string);

type term =
  | Term_marker
  | Term_unit
  | Term_var(string)
  | Term_abs(ty, option(string), term)
  | Term_app(term, term)
  
  /* this is where the "derived forms" begin */
  | Term_let_in(string, term, term)
  | Term_type_in(string, ty, term);

type ast =
  | Ast_marker
  | Ast_unit
  | Ast_var(int)
  | Ast_abs(ty, option(string), ast)
  | Ast_app(ast, ast);

let ty_unit = () => Ty_unit;
let ty_lam = (lhs, rhs) => Ty_lam(lhs, rhs);
let ty_named = (name) => Ty_named(name);

let unit = () => Term_unit;
let marker = () => Term_marker;
let var = (name) => Term_var(name);
let abs = (ty, name, body) => Term_abs(ty, name, body);
let app = (callee, parm) => Term_app(callee, parm);
let let_in = (name, init, body) => Term_let_in(name, init, body);
let type_in = (name, ty, body) => Term_type_in(name, ty, body);

let rec string_of_ty = (ty) => {
  let is_cmplx = (ty) =>
    switch ty {
    | Ty_unit => true
    | Ty_named(_) => true
    | Ty_lam(_, _) => false
    };
  switch ty {
  | Ty_unit => "unit"
  | Ty_named(name) => name
  | Ty_lam(lhs, rhs) =>
    let lhs' =
      if (is_cmplx(lhs)) {
        "(" ++ string_of_ty(lhs) ++ ") -> "
      } else {
        string_of_ty(lhs) ++ " -> "
      };
    lhs' ++ string_of_ty(rhs)
  }
};

let print_ty = (ty) => print_string(string_of_ty(ty));

let rec print_term = (term) => {
  let is_cmplx =
    fun
    | Term_marker => false
    | Term_unit => false
    | Term_var(_) => false
    | _ => true;
  let print_cmplx = (tm) =>
    if (is_cmplx(tm)) {
      print_char('('); print_term(tm); print_char(')');
    } else {
      print_term(tm);
    };
  switch term {
  | Term_marker => print_char('@')
  | Term_unit => print_string("()")
  | Term_var(name) => print_string(name)
  | Term_abs(ty, var, body) =>
    print_char('/');
    switch (var) {
    | Some(var) => print_string(var)
    | None => print_char('_')
    };
    print_char(':');
    print_ty(ty);
    print_char('.');
    print_term(body)
  | Term_app(callee, parm) =>
    print_cmplx(callee);
    print_char(' ');
    print_cmplx(parm)
  | Term_let_in(name, init, body) =>
    print_string("let " ++ name ++ " = ");
    print_cmplx(init);
    print_string(" in\n");
    print_cmplx(body)
  | Term_type_in(name, ty, body) =>
    print_string("type " ++ name ++ " = ");
    print_ty(ty);
    print_string(" in\n");
    print_cmplx(body)
  }
};

let string_of_ast = (ast) => {
  let rec bound_var_name = (idx, names) =>
    switch names {
    | [name, ...names] =>
      if (idx == 0) {
        switch name {
        | Some(name) => name
        | None => failwith("malformed lambda calculus ast")
        }
      } else {
        bound_var_name(idx - 1, names)
      }
    | [] => failwith("malformed lambda calculus ast")
    };
  let is_cmplx = (ast) =>
    switch ast {
    | Ast_marker => false
    | Ast_unit => false
    | Ast_var(_) => false
    | _ => true
    };
  let rec soa_rec = (ast, names) => {
    let soa_cmplx = (ast) =>
      if (is_cmplx(ast)) {
        "(" ++ soa_rec(ast, names) ++ ")"
      } else {
        soa_rec(ast, names)
      };
    switch ast {
    | Ast_marker => "@"
    | Ast_unit => "()"
    | Ast_var(idx) => bound_var_name(idx, names)
    | Ast_app(callee, parm) => soa_cmplx(callee) ++ " " ++ soa_cmplx(parm)
    | Ast_abs(ty, name, body) =>
      let name' = switch name {
      | Some(name) => name
      | None => "_"
      };
      "/" ++ name' ++ ":" ++ string_of_ty(ty) ++ "." ++ soa_rec(body, [name, ...names])
    }
  };
  soa_rec(ast, [])
};

let print_ast = (ast) => print_string(string_of_ast(ast));

type type_error =
  | Type_error_variable_not_found(string)
  | Type_error_type_not_found(string)
  | Type_error_incorrect_types(ty, ty)
  | Type_error_calling_non_callable(ty, ast);

let print_type_error = (err) =>
  switch err {
  | Type_error_variable_not_found(var) => print_string("variable not found: " ++ var)
  | Type_error_type_not_found(ty) => print_string("type not found: " ++ ty)
  | Type_error_incorrect_types(lhs, rhs) =>
    print_string("type mismatch: " ++ string_of_ty(lhs) ++ " != " ++ string_of_ty(rhs))
  | Type_error_calling_non_callable(ty, ast) =>
    print_string(
      "attempt to call non-callable: " ++ string_of_ty(ty) ++ "\n  " ++ string_of_ast(ast)
    )
  };

let rec typeof_rec = (ast, tys) =>
  switch ast {
  | Ast_app(callee, _) as ast' =>
    switch (typeof_rec(callee, tys)) {
    | Result.Ok(Ty_lam(_, ret)) => Result.Ok(ret)
    | Result.Ok(Ty_named(_)) => failwith("malformed ast")
    | Result.Ok(ty) => Result.Err(Type_error_calling_non_callable(ty, ast'))
    | Result.Err(e) => Result.Err(e)
    }
  | Ast_abs(ty, _, body) =>
    typeof_rec(body, [ty, ...tys])
    >>= (it) => pure(Ty_lam(ty, it))
  | Ast_unit => Result.Ok(Ty_unit)
  | Ast_marker => Result.Ok(Ty_lam(Ty_unit, Ty_unit))
  | Ast_var(idx) => Result.Ok(List.nth(tys, idx))
  };

let typeof = (ast) =>
  switch (typeof_rec(ast, [])) {
  | Result.Ok(Ty_named(_)) => failwith("malformed ast")
  | Result.Ok(ty) => ty
  | Result.Err(_) => failwith("malformed ast")
  };

let finish = (tm) => {
  let rec get_var = (name, names, idx) =>
    switch names {
    | [x, ..._] when x == Some(name) => Result.Ok(Ast_var(idx))
    | [_, ...xs] => get_var(name, xs, idx + 1)
    | [] => Result.Err(Type_error_variable_not_found(name))
    };
  let rec get_structural_ty = (ty, ty_names) => {
    let rec get_named_ty = (name, ty_names) =>
      switch ty_names {
      | [(name', ty), ..._] when name' == name => Result.Ok(ty)
      | [_, ...xs] => get_named_ty(name, xs)
      | [] => Result.Err(Type_error_type_not_found(name))
      };

    switch ty {
    | Ty_named(name) => get_named_ty(name, ty_names)
    | Ty_lam(lhs, rhs) =>
      get_structural_ty(lhs, ty_names)
      >>= (lhs) => get_structural_ty(rhs, ty_names)
      >>= (rhs) => pure(Ty_lam(lhs, rhs))
    | Ty_unit => pure(Ty_unit)
    }
  };
  let rec finish_rec = (tm, names, tys, ty_names) =>
    switch tm {
    | Term_marker => Result.Ok(Ast_marker)
    | Term_unit => Result.Ok(Ast_unit)
    | Term_var(name) => get_var(name, names, 0)
    | Term_app(callee, parm) =>
      finish_rec(callee, names, tys, ty_names)
      >>= (callee') => finish_rec(parm, names, tys, ty_names)
      >>= (parm') => typeof_rec(callee', tys)
      >>= (it) =>
        switch it {
        | Ty_lam(parm_ty, _) =>
          typeof_rec(parm', tys)
          >>= (parm_ty') =>
            if (parm_ty != parm_ty') {
              Result.Err(Type_error_incorrect_types(parm_ty, parm_ty'))
            } else {
              Result.Ok(Ast_app(callee', parm'))
            }
        | Ty_named(_) => failwith("substitution of named types failed")
        | ty =>
          Result.Err(
            Type_error_calling_non_callable(ty, Ast_app(callee', parm'))
          )
        }
    | Term_abs(ty, name, body) =>
      get_structural_ty(ty, ty_names)
      >>= (ty) => finish_rec(body, [name, ...names], [ty, ...tys], ty_names)
      >>= (body') => pure(Ast_abs(ty, name, body'))
    | Term_let_in(name, init, body) =>
      finish_rec(init, names, tys, ty_names)
      >>= (init') => {
        let init_ty = typeof(init');
        finish_rec(body, [Some(name), ...names], [init_ty, ...tys], ty_names)
        >>= (body') =>
          pure(Ast_app(
            Ast_abs(init_ty, Some(name), body'),
            init'
          ))
      }
    | Term_type_in(name, ty, body) =>
      get_structural_ty(ty, ty_names)
      >>= (ty) => finish_rec(body, names, tys, [(name, ty), ...ty_names])
    };
  finish_rec(tm, [], [], [])
};

let substitute = (body, parm) => {
  let rec sub_rec = (body, parm, idx) =>
    switch body {
    | Ast_var(idx') when idx == idx' => parm
    | Ast_app(callee', parm') => Ast_app(sub_rec(callee', parm, idx), sub_rec(parm', parm, idx))
    | Ast_abs(ty, name, body') => Ast_abs(ty, name, sub_rec(body', parm, idx + 1))
    | unchanged => unchanged
    };
  sub_rec(body, parm, 0)
};

let rec eval1 = (ast) => {
  let rec eval_app = (callee, parm) =>
    switch callee {
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
    };
  switch ast {
  | Ast_marker => None
  | Ast_unit => None
  | Ast_var(_) => failwith("malformed ast")
  | Ast_abs(_, _, _) => None
  | Ast_app(callee, parm) => eval_app(callee, parm)
  }
};
