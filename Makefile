all: deps compile

deps: deps/dk_yaws

deps/dk_yaws:
	@./rebar get-deps

compile:
	@./rebar compile

dialyze: all
	dialyzer -Wrace_conditions bubba/ebin

gc:
	@echo 'Removing all emacs backup files'
	@find . -name "*~" -exec rm -f {} \;
	@find . -name "erl_crash.dump" -exec rm -f {} \;

rel: all
	@echo 'Generating bubba release'
	@(cd rel; ../rebar generate)

clean: gc
	@./rebar clean

relclean:
	@rm -f rel/erl_crash.dump
	@rm -rf rel/bubba

realclean: clean relclean
	@./rebar del-deps
	@rm -rf deps/*

test: all
	ERL_LIBS=$(CURDIR):$(CURDIR)/deps ./rebar skip_deps=true eunit

eunit:
	make test
