DOCKER=docker-compose -f docker-compose.yml run --rm --service-ports swagger_tester
REBAR=$(DOCKER) rebar3

.PHONY: all compile test clean

all: compile

compile:
	$(REBAR) compile

test:
	$(REBAR) do dialyzer, xref, eunit, ct, cover

clean:
	$(REBAR) clean --all
	$(DOCKER) rm -Rf test_log
	$(DOCKER) rm -Rf _build/test

distclean: clean
	$(DOCKER) rm -Rf _build

upgrade:
	$(REBAR) upgrade

shell:
	$(REBAR) shell
