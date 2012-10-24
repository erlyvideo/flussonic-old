
all:
	@./rebar compile

clean:
	@./rebar clean
	@rm -f erl_crash.dump

test:
	@./rebar compile eunit


.PHONY: test

PLT_NAME=.erlmedia_dialyzer.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $@ \
		--apps kernel stdlib crypto || true

dialyze: $(PLT_NAME)
	@dialyzer ebin  --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wunderspecs
