REBAR ?= rebar3
PROJECT := hooks

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
