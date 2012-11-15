all:
	ERL_LIBS=..:../../deps ../../rebar compile

test:
	ERL_LIBS=..:../../deps ../../rebar -v eunit

clean:
	ERL_LIBS=..:../../deps ../../rebar clean

bench:
	ERL_LIBS=..:../../deps ../../rebar eunit
	cd .eunit && ERL_LIBS=../..:../../../deps erl -pa . -s mpegts_decoder_tests bench -s init stop -noshell

.PHONY: test

