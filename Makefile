# Overview of make targets:
#
#   all         Compile app and escript (the default target)
#   app         Compile the OTP application (beams and app file)
#   shell       Start an Erlang shell with gradualizer in the code path
#   eunit       Run eunit tests
#   tests       Run all tests (eunit)
#   cover       Run tests and generate coverage reports (XML and plain)
#   check       Run all checks (tests, dialyze, gradualize)
#   clean       Delete all generated files
#
# Capitalized variables can be overriden on the command line.
# Example:
#
#   make tests EUNIT_OPTS=      Run tests without the eunit verbose option
#

APP=atomcheck

.PHONY: all
all: app

# Compilation
# -----------

erls    = $(wildcard src/*.erl)
beams   = $(erls:src/%.erl=ebin/%.beam)
app_src = $(wildcard src/*.app.src)
app     = $(app_src:src/%.app.src=beam/%.app)

ERLC_OPTS = -I include -pa ebin +debug_info

.PHONY: app
app: $(beams) $(app)

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl | ebin
	erlc $(ERLC_OPTS) -o ebin $<

# Compile-time dependencies between modules and other files,
# e.g. parse transforms

#ebin/some_module.beam: ebin/some_parse_transform.beam

ebin/%.app: src/%.app.src | ebin
	sed s/VERSION/$$(git describe --tags --always)/ $< > $@

# shell, clean
# ------------

.PHONY: shell
shell: app
	erl -pa ebin

.PHONY: clean
clean:
	rm -rf ebin cover test/*.beam

# Tests
# -----

.PHONY: tests eunit compile-tests
tests: eunit

EUNIT_OPTS=

test_erls=$(wildcard test/*.erl)
test_beams=$(test_erls:test/%.erl=test/%.beam)

compile-tests: app $(test_beams) test/any.beam test/records.beam

test/%.beam: test/%.erl
	erlc $(ERLC_OPTS) -o test $<

# Extra beams used by some test cases
test/any.beam: test/should_pass/any.erl
	erlc $(ERLC_OPTS) -o test $<

test/records.beam: test/should_pass/records.erl
	erlc $(ERLC_OPTS) -o test $<

define erl_run_eunit
case eunit:test("test", [$(EUNIT_OPTS)]) of \
    ok -> ok; \
    error -> halt(2) \
end
endef

eunit: compile-tests
	erl -noinput -pa ebin -pa test -eval \
	 '$(erl_run_eunit), halt().'

.PHONY: cover
cover: compile-tests
	mkdir -p cover
	erl -noinput -pa ebin -pa test -eval \
	 '%% Cover compile, run eunit, export and generate reports \
	  case cover:compile_beam_directory("ebin") of % \
	      {error, _} -> halt(2); % \
	      _List      -> ok % \
	  end, % \
	  $(erl_run_eunit), % \
	  cover:export("cover/eunit.coverdata"), % \
	  cover:analyse_to_file([{outdir, "cover"}]), % plain text \
	  cover:analyse_to_file([{outdir, "cover"}, html]), % \
	  halt().'

cover/coverage.xml: cover test/covertool.beam
	erl -noinput -pa test -eval \
	 'covertool:main(["-cover", "cover/eunit.coverdata", % \
	                  "-output", "cover/coverage.xml", % \
	                  "-appname", "$(app:ebin/%.app=%)"]), % \
	  halt().'

test/covertool.beam: test/covertool.erl test/covertool.hrl
	erlc $(ERLC_OPTS) -I test -o test $<

# Download the deps for generating XML cover report
test/covertool.erl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.1/src/covertool.erl \
	     -o $@

test/covertool.hrl:
	curl -Ls https://github.com/covertool/covertool/raw/2.0.1/include/covertool.hrl \
	     -o $@

# Dialyze
# -------

DIALYZER_PLT = .dialyzer_plt
export DIALYZER_PLT
PLT_APPS = erts kernel stdlib syntax_tools
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions
               # -Wunmatched_returns -Wunknown
               # -Wunderspecs -Woverspec -Wspecdiffs

.PHONY: dialyze
dialyze: app $(DIALYZER_PLT)
	dialyzer $(DIALYZER_OPTS) ebin

# DIALYZER_PLT is a variable understood directly by Dialyzer.
# Exit status 2 = warnings were emitted
$(DIALYZER_PLT):
	dialyzer --build_plt --apps $(PLT_APPS) || test $$? -eq 2

.PHONY: check
check: tests dialyze
