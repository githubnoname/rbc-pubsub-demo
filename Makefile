.PHONY: build run check-rebar3

check-%:
	@command -v $* &>/dev/null || (echo You have to install $*; exit 1)

build: check-rebar3
	rebar3 release

run:
	_build/default/rel/rbc_pubsub_demo/bin/rbc_pubsub_demo console
