# Pinerl
Pinerl brings the [Pin operator][1] from the Elixir language by the means of a
parse transform. Use of a pin operator in Erlang code makes the programmer's
intent explicit, making it easier to read. It also discoverers a real and common
mistake in Erlang code -- unintentional matching -- that are not found by any
other warnings.

As the implementation is a parse transform, the syntax for the Pin operator must
be syntactically valid Erlang, but preferably semantically invalid. The current
implementation uses a code-block for this purpose:

    X = 1,
    begin X end = 3.

Although it is strongly recommended to use the `?PIN(X)` macro, which can be
included from `pinerl.hrl`:

    -define(PIN(X), define X end).

 [1] http://elixir-lang.org/getting-started/pattern-matching.html#the-pin-operator
