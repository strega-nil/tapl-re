type t('t, 'e) =
  | Ok('t)
  | Err('e);

let map = (lam, res) =>
  switch res {
  | Ok(r) => Ok(lam(r))
  | Err(_) as err => err
  };

let bind = (lam, res) =>
  switch res {
  | Ok(r) => lam(r)
  | Err(_) as err => err
  };

module Monad = {
  let (>>=) = (x, y) => bind(y, x);
  let pure = (x) => Ok(x);
};
