.PHONY: clean eunit ct

clean:
	rebar -r clean skip_deps=true

eunit:
	rebar -r compile skip_deps=true
	rebar -r eunit skip_deps=true

ct:
	rebar -r compile skip_deps=true
	rebar -r ct skip_deps=true

release:
	relx
