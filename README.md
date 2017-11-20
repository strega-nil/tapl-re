Types And Programming Languages
===============================

My implementation of a lambda calculus. It's fairly simple, it's simply typed,
and written in reasonML.

The type system includes only `unit` and `->`. There are two constants;
`(): unit`, and `@: unit -> unit`. `@` is only included for printing; an application of `@` is not evaluated. This is useful for printing numbers;
`n @ ()`, which would print as if `()` is `Z`, while `@` is `S`. Similarly, `true` can be implemented as `@ ()`, versus `false` is `()`.
