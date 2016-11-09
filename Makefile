all: compile

compile: deps/neotoma/ebin/neotoma.beam
	./rebar compile skip_deps=true

deps/neotoma/ebin/neotoma.beam: 
	./rebar get-deps compile

clean:
	./rebar clean skip_deps=true
	rm -f src/mysql_proto.erl

test: deps/neotoma/ebin/neotoma.beam
	./rebar ct skip_deps=true
	./covertool -cover myproto.coverdata -appname myproto -output cobertura.xml

.PHONY: test compile clean all
