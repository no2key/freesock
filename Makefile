REBAR=rebar

.PHONY: compile all test clean

compile:
	$(REBAR) skip_deps=true compile

all:
	$(REBAR) get-deps && ${REBAR} compile

test:
	ERL_FLAGS="-config rel/files/sys" $(REBAR) skip_deps=true eunit

clean:
	$(REBAR) skip_deps=true clean
