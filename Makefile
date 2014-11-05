.PHONY: clean eunit ct

clean:
	rebar -r clean skip_deps=true

eunit:
	rebar -r eunit skip_deps=true

ct:
	rebar -r ct skip_deps=true
