# Pinerl
Pinerl brings the
[Pin operator](http://elixir-lang.org/getting-started/pattern-matching.html#the-pin-operator)
from the Elixir language to Erlang by the means of a parse transform. The pin
operator must be applied to any variables in patterns that are matched by value,
rather than bound there. In Elixir, the pin operator is a necessity to be able
to match existing values, since the default behaviour is to shadow the old
value. That is, of course, not the case for Erlang, but the use of the pin
operator makes the programmer's intent explicit, making it easier to read. It
also discoverers a real and common mistake in Erlang code &ndash; unintentional
matching &ndash; that is not found by any other warnings.

When compiled with Pinerl, code like this will give you a warning

    X0 = math:pi(),
    Y0 = math:e(),
    X0 = Y0 + X0,

    sample:3: Warning: variable 'X0' matched without pin

Reminding you that you either should have either used a new name `X1` for your
new value, or the pin operator, depending on whether you intended to match. Here
is the same code but with the pin operator applied, making the authors intent to
match explicit:

    X0 = math:pi(),
    Y0 = math:e(),
    ?PIN(X0) = Y0 + X0,

Of course, using the pin operator on a new binding also gives you a warning:

    X0 = math:pi(),
    Y0 = math:e(),
    ?PIN(X1) = Y0 + X0,

    sample:3: Warning: variable 'X1' is unbound in pin

As the implementation is a parse transform, the syntax for the Pin operator must
be syntactically valid Erlang, but preferably semantically invalid. The current
implementation uses a code-block for this purpose:

    X = 1,
    begin X end = 3.

Although it is strongly recommended to use the `?PIN(X)` macro, which can be
included from `pinerl.hrl`:

    -define(PIN(X), begin X end).

# How to Use
It is recommended you use Pinerl in your entire project to minimise
confusion. Therefore, you should include the parse transform in your compiler
options (`"+{parse_transform,pinerl_transform}"`). In any files where the
`?PIN(X)` macro is needed, include the header:

    -include_lib("pinerl/include/pinerl.hrl").

## Erlang.mk
Add the following to your Makefile:

    DEPS += pinerl
    dep_pinerl = git git://github.com/margnus1/pinerl.git master
    ERLC_OPTS += "+{parse_transform,pinerl_transform}"

Beware that if you do not otherwise define `ERLC_OPTS`, you will override the
default options provided by Erlang.mk. You can add the following line (taken
from `erlang.mk`) *before* the previous to set the default options explicitly:

    ERLC_OPTS ?= -Werror +debug_info +warn_export_vars +warn_shadow_vars \
	    +warn_obsolete_guard # +bin_opt_info +warn_export_all +warn_missing_spec

## Rebar
Add the following to your `deps` and `erl_opts` lists in `rebar.config`:

    {deps, [{pinerl, ".*",
             {git, "git://github.com/margnus1/pinerl.git", {branch, "master"}}}
           ]}.
    {erl_opts, [{parse_transform,pinerl_transform}]}.
