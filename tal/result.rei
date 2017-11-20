type t('t, 'e) =
  | Ok('t)
  | Err('e);

let map: ('t => 'u, t('t, 'e)) => t('u, 'e);

let bind: ('t => t('u, 'e), t('t, 'e)) => t('u, 'e);

module Monad: {
  let (>>>): (t('t, 'e), 't => 'u) => t('u, 'e);
  let (>>=): (t('t, 'e), 't => t('u, 'e)) => t('u, 'e);
};
