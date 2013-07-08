REBAR=`which rebar`

all:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean

.PHONY: test

test:
	@$(REBAR) eunit skip_deps=true

bench:
	@$(REBAR) compile skip_deps=true
	erl -pa ebin -s exomler bench

PLT_NAME=.exomler.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $@ \
		--apps kernel stdlib crypto || true

dialyze: $(PLT_NAME)
	@dialyzer ebin --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wunderspecs
