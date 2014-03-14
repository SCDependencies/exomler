REBAR=`which rebar`

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

.PHONY: test

test:
	@$(REBAR) eunit skip_deps=true

PLT_NAME=.exomler.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $@ \
		--apps kernel stdlib crypto || true

dialyze: $(PLT_NAME)
	@dialyzer ebin --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wunderspecs
