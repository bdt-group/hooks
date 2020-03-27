REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) ct

.PHONY: test dialyzer clean

dialyzer:
	@$(REBAR) dialyzer
