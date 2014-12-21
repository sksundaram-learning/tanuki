.PHONY: clean compile eunit ct dev rel

clean:
	rebar -r clean skip_deps=true

compile:
	$(MAKE) -C apps/tanuki_backend copy-static
	rebar -r compile skip_deps=true

eunit: compile
	rebar -r eunit skip_deps=true

ct: compile
	rebar -r ct skip_deps=true

dev: compile
	relx --dev-mode --relname tanuki --relvsn dev

rel: compile
	relx
