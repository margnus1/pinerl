PROJECT=pinerl
include erlang.mk

all:: test dogfood dialyze

dogfood:
	@echo " PINERL "src/*.erl
	@src/pinerl_check -pa ebin -Dpinerl_transform_dogfood -Iinclude src/*.erl

test:
	@echo " TEST   "tests/*.erl
	@src/pinerl_check -pa ebin -Iinclude tests/*.erl \
		| diff -u - tests/expected.txt

.PHONY: dogfood test
