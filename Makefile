
all:
	./rebar get-deps compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

.PHONY: test
