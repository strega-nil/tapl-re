type t('t, 'e) =
| Ok('t)
| Err('e);

let bind = (lam, res) => switch (res) {
| Ok(r) => lam(r)
| Err(_) as err => err
};

let map = (lam, res) => switch (res) {
| Ok(r) => Ok(lam(r))
| Err(_) as err => err
};
