.PHONY: clean test

clean:
	rebar -r clean skip_deps=true

test:
	rebar -r eunit skip_deps=true
