type parser_error;

let print_parser_error: parser_error => unit;

let parse: string => Result.t(Lambda.term, parser_error);
