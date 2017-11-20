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

let rec print_ty = (ty) => {
  let is_unit = (ty) => {
    switch (ty) {
    | Ty_unit => true
    | Ty_lam(_, _) => false
    }
  };

  switch (ty) {
  | Ty_unit => print_string("unit")
  | Ty_lam(lhs, rhs) =>
    if (is_unit(lhs)) {
      print_string("unit -> ");
    } else {
      print_char('(');
      print_ty(lhs);
      print_string(") -> ");
    };
    print_ty(rhs)
  }
};

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

let print_ast = (ast) => {
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

  let rec print_ast_rec = (ast, names) => {
    let print_cmplx = (ast) => {
      if (is_cmplx(ast)) {
        print_char('('); print_ast_rec(ast, names); print_char(')')
      } else {
        print_ast_rec(ast, names)
      }
    };
    switch (ast) {
    | Ast_marker => print_char('@')
    | Ast_unit => print_string("()")
    | Ast_var(idx) => print_string(bound_var_name(idx, names))
    | Ast_app(callee, parm) =>
      print_cmplx(callee);
      print_char(' ');
      print_cmplx(parm)
    | Ast_abs(ty, name, body) =>
      print_char('/');
      print_string(name);
      print_char(':');
      print_ty(ty);
      print_char('.');
      print_ast_rec(body, [name, ...names])
    }
  };

  print_ast_rec(ast, [])
};

exception Type_error_variable_not_found(string);
exception Type_error_incorrect_types(ty, ty);
exception Type_error_calling_non_callable(ty);
let finish = (tm) => {
  let rec get_var = (name, names, idx) => switch (names) {
  | [x, ..._] when x == name => Ast_var(idx)
  | [_, ...xs] => get_var(name, xs, idx + 1)
  | [] => raise(Type_error_variable_not_found(name))
  };
  let rec typeof = (ast, tys) => switch (ast) {
  | Ast_app(callee, _) =>
    switch (typeof(callee, tys)) {
    | Ty_lam(_, ret) => ret
    | ty => raise(Type_error_calling_non_callable(ty))
    }
  | Ast_abs(ty, _, body) => Ty_lam(ty, typeof(body, [ty, ...tys]))
  | Ast_unit => Ty_unit
  | Ast_marker => Ty_lam(Ty_unit, Ty_unit)
  | Ast_var(idx) => List.nth(tys, idx)
  };
  let rec finish_rec = (tm, names, tys) => {
    switch (tm) {
    | Term_marker => Ast_marker
    | Term_unit => Ast_unit
    | Term_var(name) => get_var(name, names, 0)
    | Term_app(callee, parm) =>
      let callee' = finish_rec(callee, names, tys);
      let parm' = finish_rec(parm, names, tys);
      switch (typeof(callee', tys)) {
      | Ty_lam(parm_ty, _) =>
        let parm_ty' = typeof(parm', tys);
        if (parm_ty != parm_ty') {
          raise(Type_error_incorrect_types(parm_ty, parm_ty'));
        } else {
          Ast_app(callee', parm')
        }
      | ty => raise(Type_error_calling_non_callable(ty))
      }
    | Term_abs(ty, name, body) =>
      Ast_abs(ty, name, finish_rec(body, [name, ...names], [ty, ...tys]))
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
    | Ast_marker => None
    | Ast_unit => None
    | Ast_var(_) => failwith("malformed lambda ast")
    | Ast_abs(_, _, body) => Some(substitute(body, parm))
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
