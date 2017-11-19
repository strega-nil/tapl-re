open Lambda;

type token =
| Tok_lambda
| Tok_dot
| Tok_colon
| Tok_arrow
| Tok_unit
| Tok_open_paren
| Tok_close_paren
| Tok_var(string);

type lexer = {
  buffer: string,
  mutable idx: int,
  mutable peekahead: option(token),
};

exception Lexer_error_unrecognized_character;

let is_whitespace = (ch) => {
  ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
};

let is_alpha = (ch) => {
  (ch >= 'A' && ch <= 'Z')
  || (ch >= 'a' && ch <= 'z')
};
let is_ident_start = (ch) => {
  is_alpha(ch) || ch == '_'
};
let is_ident_continue = (ch) => {
  is_ident_start(ch) || ch == '\''
};

let make_lexer = (buff) => { buffer: buff, idx: 0, peekahead: None };

let next_token = (lex) => {
  let peek_ch = () => {
    if (String.length(lex.buffer) <= lex.idx) {
      None
    } else {
      Some(String.get(lex.buffer, lex.idx))
    }
  };
  let next_ch = () => {
    switch (peek_ch()) {
    | Some(ch) => { lex.idx = lex.idx + 1; Some(ch) }
    | None => None
    }
  };
  let eat_ch = () => { next_ch() |> ignore; };
  let rec eat_whitespace = () => {
    switch (peek_ch()) {
    | Some(ch) when is_whitespace(ch) => eat_ch(); eat_whitespace();
    | Some(_) | None => ()
    }
  };
  let lex_ident = (fst) => {
    let rec helper = (bytes, idx) => {
      switch (peek_ch()) {
      | Some(ch) when is_ident_continue(ch) =>
        eat_ch();
        if (idx < Bytes.length(bytes)) {
          Bytes.set(bytes, idx, ch);
          helper(bytes, idx + 1)
        } else {
          failwith(
            "identifiers longer than "
            ++ string_of_int(Bytes.length(bytes))
            ++ " are not supported");
        }
      | Some(_) | None => idx
      }
    };
    let bytes = Bytes.create(16);
    Bytes.set(bytes, 0, fst);
    let length = helper(bytes, 1);
    let bytes' = Bytes.extend(bytes, 0, length - Bytes.length(bytes));
    Bytes.unsafe_to_string(bytes')
  };

  switch (lex.peekahead) {
  | Some(_) as peek =>
    lex.peekahead = None;
    peek
  | None =>
    eat_whitespace();
    switch (next_ch()) {
    | Some('/') => Some(Tok_lambda)
    | Some(':') => Some(Tok_colon)
    | Some('.') => Some(Tok_dot)
    | Some('-') =>
      switch (next_ch()) {
      | Some('>') => Some(Tok_arrow)
      | _ => raise(Lexer_error_unrecognized_character)
      }
    | Some('(') => Some(Tok_open_paren)
    | Some(')') => Some(Tok_close_paren)
    | Some(ch) when is_ident_start(ch) =>
      let res = lex_ident(ch);
      if (res == "unit") {
        Some(Tok_unit)
      } else {
        Some(Tok_var(lex_ident(ch)))
      }
    | Some(ch) => raise(Lexer_error_unrecognized_character);
    | None => None
    }
  }
};

let peek_token = (lex) => {
  lex.peekahead = next_token(lex);
  lex.peekahead
};

let eat_token = (lex) => next_token(lex) |> ignore;

exception Parser_error_unexpected_token;
exception Parser_error_unexpected_eof;

let rec maybe_parse_term = (lex) => {
  let get_var = () => {
    switch (next_token(lex)) {
    | Some(Tok_var(name)) => name
    | Some(_) => raise(Parser_error_unexpected_token)
    | None => raise(Parser_error_unexpected_eof)
    }
  };
  let get_dot = () => {
    switch (next_token(lex)) {
    | Some(Tok_dot) => ()
    | Some(_) => raise(Parser_error_unexpected_token)
    | None => raise(Parser_error_unexpected_eof)
    }
  };
  let get_colon = () => {
    switch (next_token(lex)) {
    | Some(Tok_colon) => ()
    | Some(_) => raise(Parser_error_unexpected_token)
    | None => raise(Parser_error_unexpected_eof)
    }
  };
  let get_close_paren = () => {
    switch (next_token(lex)) {
    | Some(Tok_close_paren) => ()
    | Some(_) => raise(Parser_error_unexpected_token)
    | None => raise(Parser_error_unexpected_eof)
    }
  };
  let get_ty = () => {
    switch(next_token(lex)) {
    | Some(Tok_unit) => Lambda.ty_unit()
    | Some(Tok_open_paren) => failwith("unimplemented")
    | Some(_) => raise(Parser_error_unexpected_token)
    | None => raise(Parser_error_unexpected_eof)
    }
  };
  let rec parse_app_list = (fst) => {
    switch (peek_token(lex)) {
    | None | Some(Tok_close_paren) => fst
    | Some(Tok_open_paren) | Some(Tok_lambda) =>
      Lambda.app(fst, parse_term(lex))
    | Some(Tok_var(snd)) =>
      eat_token(lex);
      parse_app_list(Lambda.app(fst, Lambda.var(snd)))
    | Some(_) =>
      raise(Parser_error_unexpected_token)
    }
  };

  switch (peek_token(lex)) {
  | None => None
  | Some(Tok_close_paren) => None
  | Some(Tok_lambda) =>
    eat_token(lex);
    let name = get_var();
    get_colon();
    let ty = get_ty();
    get_dot();
    let body = parse_term(lex);
    Some(Lambda.abs(ty, name, body))
  | Some(Tok_var(name)) =>
    eat_token(lex);
    Some(parse_app_list(Lambda.var(name)))
  | Some(Tok_open_paren) =>
    /* todo: implement unit */
    eat_token(lex);
    let ret = parse_term(lex);
    get_close_paren();
    Some(parse_app_list(ret))
  | Some(_) => raise(Parser_error_unexpected_token);
  }
} and parse_term = (lex) => {
  switch (maybe_parse_term(lex)) {
  | Some(tm) => tm
  | None =>
    switch (next_token(lex)) {
    | None => raise(Parser_error_unexpected_eof)
    | Some(_) => raise(Parser_error_unexpected_token)
    }
  }
};

let parse = (buff) => {
  let is_some = (opt) => switch (opt) {
  | Some(_) => true
  | None => false
  };
  let lex = make_lexer(buff);
  let ret = parse_term(lex);
  if (is_some(next_token(lex))) {
    failwith("parser didn't eat all the input");
  };
  ret
};
