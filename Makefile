REBAR ?= rebar3
PROJECT := hooks
BUILD_IMAGE  ?= gitlab.bdt.tools:5000/build-ubuntu1804:1.4.2

.PHONY: compile clean distclean xref dialyzer dialyze linter lint

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean: clean
	rm -rf _build

xref:
	@$(REBAR) xref

dialyzer:
	@$(REBAR) dialyzer

dialyze:
	@$(REBAR) dialyzer

linter:
	@$(REBAR) as lint lint

lint:
	@$(REBAR) as lint lint

.PHONY: d_%

d_%:
	./build-with-env --image $(BUILD_IMAGE) make $*
