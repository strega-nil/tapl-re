open Lambda;

open Result.Monad;

type token =
  | Tok_lambda
  | Tok_dot
  | Tok_colon
  | Tok_arrow
  | Tok_unit
  | Tok_marker
  | Tok_open_paren
  | Tok_close_paren
  | Tok_underscore
  | Tok_var(string);

let string_of_token = (tok) =>
  switch tok {
  | Tok_lambda => "/"
  | Tok_dot => "."
  | Tok_colon => ":"
  | Tok_arrow => "->"
  | Tok_unit => "unit"
  | Tok_marker => "@"
  | Tok_open_paren => "("
  | Tok_close_paren => ")"
  | Tok_underscore => "_"
  | Tok_var(name) => name
  };

type lexer = {
  buffer: string,
  mutable idx: int,
  mutable peekahead: option(token)
};

type lexer_error =
  | Lexer_error_end_of_file
  | Lexer_error_unrecognized_character(char);

let is_whitespace = (ch) => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';

let is_alpha = (ch) => ch >= 'A' && ch <= 'Z' || ch >= 'a' && ch <= 'z';

let is_ident_start = (ch) => is_alpha(ch) || ch == '_';

let is_ident_continue = (ch) => is_ident_start(ch) || ch == '\'';

let make_lexer = (buff) => {buffer: buff, idx: 0, peekahead: None};

let next_token: lexer => Result.t(token, lexer_error) =
  (lex) => {
    let peek_ch = () =>
      if (String.length(lex.buffer) <= lex.idx) {
        None
      } else {
        Some(lex.buffer.[lex.idx])
      };
    let next_ch = () =>
      switch (peek_ch()) {
      | Some(ch) =>
        lex.idx = lex.idx + 1;
        Some(ch)
      | None => None
      };
    let eat_ch = () => next_ch() |> ignore;
    let rec eat_whitespace = () =>
      switch (peek_ch()) {
      | Some(ch) when is_whitespace(ch) =>
        eat_ch();
        eat_whitespace()
      | Some(_)
      | None => ()
      };
    let lex_ident = (fst) => {
      let rec helper = (bytes, idx) =>
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
              ++ " are not supported"
            )
          }
        | Some(_)
        | None => idx
        };
      let bytes = Bytes.create(16);
      Bytes.set(bytes, 0, fst);
      let length = helper(bytes, 1);
      let bytes' = Bytes.extend(bytes, 0, length - Bytes.length(bytes));
      Bytes.unsafe_to_string(bytes')
    };
    switch lex.peekahead {
    | Some(peek) =>
      lex.peekahead = None;
      Result.Ok(peek)
    | None =>
      eat_whitespace();
      switch (next_ch()) {
      | Some('/') => Result.Ok(Tok_lambda)
      | Some(':') => Result.Ok(Tok_colon)
      | Some('.') => Result.Ok(Tok_dot)
      | Some('-') =>
        switch (next_ch()) {
        | Some('>') => Result.Ok(Tok_arrow)
        | _ => Result.Err(Lexer_error_unrecognized_character('-'))
        }
      | Some('@') => Result.Ok(Tok_marker)
      | Some('(') => Result.Ok(Tok_open_paren)
      | Some(')') => Result.Ok(Tok_close_paren)
      | Some(ch) when is_ident_start(ch) =>
        let res = lex_ident(ch);
        if (res == "unit") {
          Result.Ok(Tok_unit)
        } else if (res == "_") {
          Result.Ok(Tok_underscore)
        } else {
          Result.Ok(Tok_var(lex_ident(ch)))
        }
      | Some(ch) => Result.Err(Lexer_error_unrecognized_character(ch))
      | None => Result.Err(Lexer_error_end_of_file)
      }
    }
  };

let peek_token = (lex) => {
  next_token(lex) >>>
  (tok) => {
    lex.peekahead = Some(tok);
    tok
  }
};

let eat_token = (lex) => next_token(lex) |> ignore;

type parser_error =
  | Parser_error_unrecognized_character(char)
  | Parser_error_unexpected_token(token)
  | Parser_error_unexpected_eof;

let print_parser_error = (err) =>
  switch err {
  | Parser_error_unrecognized_character(ch) =>
    Printf.printf("unrecognized character: %c (%d)", ch, int_of_char(ch))
  | Parser_error_unexpected_token(tok) =>
    print_string("unexpected token: " ++ string_of_token(tok))
  | Parser_error_unexpected_eof => print_string("unexpected end of file")
  };

let rec maybe_parse_term = (lex) => {
  let map_err = (e) =>
    switch e {
    | Lexer_error_unrecognized_character(ch) => Result.Err(Parser_error_unrecognized_character(ch))
    | Lexer_error_end_of_file => Result.Err(Parser_error_unexpected_eof)
    };
  /* note: returns None for _, Some(name) for anything else */
  let get_var_or_under = () =>
    switch (next_token(lex)) {
    | Result.Ok(Tok_var(name)) => Result.Ok(Some(name))
    | Result.Ok(Tok_underscore) => Result.Ok(None)
    | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
    | Result.Err(e) => map_err(e)
    };
  let get_dot = () =>
    switch (next_token(lex)) {
    | Result.Ok(Tok_dot) => Result.Ok()
    | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
    | Result.Err(e) => map_err(e)
    };
  let get_colon = () =>
    switch (next_token(lex)) {
    | Result.Ok(Tok_colon) => Result.Ok()
    | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
    | Result.Err(e) => map_err(e)
    };
  let get_close_paren = () =>
    switch (next_token(lex)) {
    | Result.Ok(Tok_close_paren) => Result.Ok()
    | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
    | Result.Err(e) => map_err(e)
    };
  let rec get_ty = () => {
    let lhs =
      switch (next_token(lex)) {
      | Result.Ok(Tok_unit) => Result.Ok(Lambda.ty_unit())
      | Result.Ok(Tok_open_paren) => get_ty() >>= ((ty) => get_close_paren() >>> (() => ty))
      | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
      | Result.Err(e) => map_err(e)
      };
    lhs >>= (lhs) =>
      switch (peek_token(lex)) {
      | Result.Ok(Tok_arrow) =>
        eat_token(lex);
        get_ty() >>> ((it) => Lambda.ty_lam(lhs, it))
      | Result.Ok(_)
      | Result.Err(Lexer_error_end_of_file) => Result.Ok(lhs)
      | Result.Err(Lexer_error_unrecognized_character(ch)) =>
        Result.Err(Parser_error_unrecognized_character(ch))
      }
  };
  let rec parse_app_list = (fst) =>
    switch (peek_token(lex)) {
    | Result.Err(Lexer_error_end_of_file)
    | Result.Ok(Tok_close_paren) => Result.Ok(fst)
    | Result.Ok(Tok_lambda) => parse_term(lex) >>> ((it) => Lambda.app(fst, it))
    | Result.Ok(Tok_open_paren) =>
      eat_token(lex);
      let snd =
        switch (peek_token(lex)) {
        | Result.Ok(Tok_close_paren) => Result.Ok(Lambda.unit())
        | Result.Ok(_) => parse_term(lex)
        | Result.Err(e) => map_err(e)
        };
      snd >>= ((snd) => get_close_paren() >>= (() => parse_app_list(Lambda.app(fst, snd))))
    | Result.Ok(Tok_var(snd)) =>
      eat_token(lex);
      parse_app_list(Lambda.app(fst, Lambda.var(snd)))
    | Result.Ok(Tok_marker) =>
      eat_token(lex);
      parse_app_list(Lambda.app(fst, Lambda.marker()))
    | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
    | Result.Err(Lexer_error_unrecognized_character(ch)) =>
      Result.Err(Parser_error_unrecognized_character(ch))
    };
  switch (peek_token(lex)) {
  | Result.Err(Lexer_error_end_of_file)
  | Result.Ok(Tok_close_paren) => Result.Ok(None)
  | Result.Ok(Tok_lambda) =>
    eat_token(lex);
    get_var_or_under()
    >>= (name) => get_colon()
    >>= () => get_ty()
    >>= (ty) => get_dot()
    >>= () => parse_term(lex)
    >>> (body) => Some(Lambda.abs(ty, name, body))
  | Result.Ok(Tok_var(name)) =>
    eat_token(lex);
    parse_app_list(Lambda.var(name))
    >>> (it) => Some(it)
  | Result.Ok(Tok_marker) =>
    eat_token(lex);
    parse_app_list(Lambda.marker())
    >>> (it) => Some(it)
  | Result.Ok(Tok_open_paren) =>
    eat_token(lex);
    switch (peek_token(lex)) {
    | Result.Ok(Tok_close_paren) =>
      eat_token(lex);
      parse_app_list(Lambda.unit())
      >>> (it) => Some(it)
    | Result.Ok(_) =>
      parse_term(lex)
      >>= (ret) => get_close_paren()
      >>= () => parse_app_list(ret)
      >>> (it) => Some(it)
    | Result.Err(e) => map_err(e)
    }
  | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
  | Result.Err(Lexer_error_unrecognized_character(ch)) =>
    Result.Err(Parser_error_unrecognized_character(ch))
  }
}
and parse_term = (lex) => {
  maybe_parse_term(lex)
  >>=
    fun
    | Some(tm) => Result.Ok(tm)
    | None =>
      switch (next_token(lex)) {
      | Result.Ok(tok) => Result.Err(Parser_error_unexpected_token(tok))
      | Result.Err(Lexer_error_end_of_file) => Result.Err(Parser_error_unexpected_eof)
      | Result.Err(Lexer_error_unrecognized_character(ch)) =>
        Result.Err(Parser_error_unrecognized_character(ch))
      }
};

let parse = (buff) => {
  let lex = make_lexer(buff);
  let ret = parse_term(lex);
  switch (next_token(lex)) {
  | Result.Err(Lexer_error_end_of_file) => ()
  | _ => failwith("parser didn't eat all the input")
  };
  ret
};
