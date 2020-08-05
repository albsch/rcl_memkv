REBAR = $(shell pwd)/rebar3
BASEDIR = $(shell pwd)
RELPATH = _build/default/rel/rcl_memkv
PRODRELPATH = _build/prod/rel/rcl_memkv
DEV1RELPATH = _build/dev1/rel/rcl_memkv
DEV2RELPATH = _build/dev2/rel/rcl_memkv
DEV3RELPATH = _build/dev3/rel/rcl_memkv
APPNAME = rcl_memkv
SHELL = /bin/bash

.PHONY: rel 

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

console: rel
	_build/default/rel/rcl_memkv/bin/rcl_memkv console ${ARGS}

rel:
	$(REBAR) release

xref: compile
	${REBAR} xref skip_deps=true

dialyzer:
	${REBAR} dialyzer


devrel1:
	$(REBAR) as dev1 release

devrel2:
	$(REBAR) as dev2 release

devrel3:
	$(REBAR) as dev3 release

devrel: devrel1 devrel2 devrel3


dev1-attach:
	$(BASEDIR)/_build/dev1/rel/rcl_memkv/bin/$(APPNAME) remote_console

dev2-attach:
	$(BASEDIR)/_build/dev2/rel/rcl_memkv/bin/$(APPNAME) remote_console

dev3-attach:
	$(BASEDIR)/_build/dev3/rel/rcl_memkv/bin/$(APPNAME) remote_console

dev1-console:
	$(BASEDIR)/_build/dev1/rel/rcl_memkv/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/rcl_memkv/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/rcl_memkv/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rcl_memkv/bin/$(APPNAME) start; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rcl_memkv/bin/$(APPNAME) stop; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/rcl_memkv/bin/$(APPNAME) eval 'riak_core:join("kv1@127.0.0.1")'; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/rcl_memkv/bin/$(APPNAME) eval 'riak_core_claimant:plan()'

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/rcl_memkv/bin/$(APPNAME) eval 'riak_core_claimant:commit()'
