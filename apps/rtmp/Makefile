all:
	ERL_LIBS=..:../../deps ./rebar compile


test:
	ERL_LIBS=..:../../deps ./rebar eunit -v 1

clean:
	./rebar clean

.PHONY: test

