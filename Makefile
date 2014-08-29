.PHONY: test

test:
	rebar -r eunit skip_deps=true
