.PHONY: deps app clean doc
PROJECT = erl-4inline

DIALYZER = dialyzer
REBAR = ./rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

doc:
	@$(REBAR) doc
