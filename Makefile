# Copyright 2015 Magnus Lång
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
PROJECT=pinerl
include erlang.mk

all:: test dogfood

dogfood:
	@echo " PINERL "src/*.erl
	@src/pinerl_check -pa ebin -Dpinerl_transform_dogfood -Iinclude src/*.erl

test:
	@echo " TEST   "tests/*.erl
	@src/pinerl_check -pa ebin -Iinclude tests/*.erl \
		| diff -u - tests/expected.txt

.PHONY: dogfood test
