
all: deps/neotoma/ebin/neotoma.beam
	./rebar compile skip_deps=true


deps/neotoma/ebin/neotoma.beam: 
	./rebar get-deps compile

clean:
	./rebar clean skip_deps=true
	rm -f src/mysql_proto.erl

test:
	./rebar eunit skip_deps=true

.PHONY: test
