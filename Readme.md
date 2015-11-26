# Pinerl
Pinerl brings the
[Pin operator](http://elixir-lang.org/getting-started/pattern-matching.html#the-pin-operator)
from the Elixir language to Erlang by the means of a parse transform. Use of a
pin operator in Erlang code makes the programmer's intent explicit, making it
easier to read. It also discoverers a real and common mistake in Erlang code
&ndash; unintentional matching &ndash; that is not found by any other warnings.

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
