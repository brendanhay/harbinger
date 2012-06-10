REBAR = `which rebar`
DEPS=deps/*/ebin

.PHONY: deps rel dev

#
# Targets
#

all: deps compile xref

compile:
	$(REBAR) compile

build:
	$(REBAR) skip_deps=true compile

deps:
	$(REBAR) get-deps

clean: devclean
	rm -rf rel/harbinger
	$(REBAR) clean
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

rel: build
	$(REBAR) generate -f

docs:
	$(REBAR) skip_deps=true doc

#
# Dev
#

console: build
	exec erl -pa apps/*/ebin $(DEPS) -sname harbinger -boot start_sasl

devclean:
	$(foreach d, $(wildcard dev/dev*), $(d)/bin/harbinger stop;)
	rm -rf dev

devrel: dev1 dev2 dev3

dev1 dev2 dev3: build
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devstart:
	$(foreach d, $(wildcard dev/dev*), $(d)/bin/harbinger start;)

devjoin:
	$(foreach d, $(wildcard dev/dev*), $(d)/bin/harbinger-admin join harbinger1@127.0.0.1;)

dev: devclean devrel devstart devjoin
	./dev/dev1/bin/harbinger-admin member_status
	./dev/dev1/bin/harbinger attach

#
# Analysis
#

PLT=./plt/R15B01.plt

WARNINGS=-Werror_handling \
	-Wrace_conditions \
	-Wunderspecs \
	-Wunmatched_returns

APPS=kernel stdlib sasl erts ssl \
	tools runtime_tools crypto \
	mnesia eunit syntax_tools compiler \
	xmerl inets edoc webtool public_key

build-plt: all
	dialyzer --build_plt --output_plt $(PLT) \
	--apps $(APPS) $(DEPS)

dialyzer: build
	dialyzer ./apps/harbinger/ebin --plt $(PLT) $(WARNINGS) \
	| grep -v 'lager_not_running'

xref:
	$(REBAR) skip_deps=true xref

typer:
	typer --annotate --plt $(PLT) -I deps/ -I cap/
