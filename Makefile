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

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

rel: all
	$(REBAR) generate

relclean:
	rm -rf rel/railgun

#
# Docs
#

docs:
	$(REBAR) skip_deps=true doc

#
# Dev
#

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/railgun/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/railgun/lib;)


stagedevrel: dev1 dev2 dev3
	$(foreach dev,$^,\
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf dev/$(dev)/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) dev/$(dev)/lib;))

devrel: dev1 dev2 dev3

devclean:
	rm -rf dev

dev1 dev2 dev3: all
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

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
