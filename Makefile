REBAR = `which rebar`
DEPS=deps/*/ebin

.PHONY: deps rel stagedevrel

#
# Targets
#

all: deps compile

compile:
	$(REBAR) compile
	$(MAKE) xref

deps:
	$(REBAR) get-deps

clean: devclean
	rm -rf rel/railgun
	$(REBAR) clean
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

rel: all
	$(REBAR) generate -f

docs:
	$(REBAR) skip_deps=true doc

#
# Dev
#

devclean:
	$(foreach d, $(wildcard dev/dev*), $(d)/bin/railgun stop;)
	rm -rf dev

devrel: dev1 dev2 dev3

dev1 dev2 dev3: all
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

devstart:
	$(foreach d, $(wildcard dev/dev*), $(d)/bin/railgun start && $(d)/bin/railgun ping;)

devjoin:
	$(foreach d, $(wildcard dev/dev*), sleep 0.5 && $(d)/bin/railgun-admin join railgun1@127.0.0.1;)

dev: devclean devrel devstart devjoin
	./dev/dev1/bin/railgun-admin member_status

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

dialyzer: compile
	dialyzer ./apps/railgun/ebin --plt $(PLT) $(WARNINGS) \
	| grep -v 'lager_not_running'

xref:
	$(REBAR) skip_deps=true xref

typer:
	typer --annotate --plt $(PLT) -I deps/ -I cap/
