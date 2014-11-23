.PHONY: clean eunit ct dev rel

clean:
	rebar -r clean skip_deps=true

eunit:
	rebar -r compile skip_deps=true
	rebar -r eunit skip_deps=true

ct:
	rebar -r compile skip_deps=true
	rebar -r ct skip_deps=true

dev:
	rebar -r compile skip_deps=true
	relx --dev-mode --relname tanuki --relvsn dev

rel:
	rebar -r compile skip_deps=true
	relx
