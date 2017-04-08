REBAR := ./rebar3

all: compile

deps:
	${REBAR} get-deps compile

compile: deps/neotoma/ebin/neotoma.beam
	${REBAR} compile skip_deps=true

deps/neotoma/ebin/neotoma.beam:
	${REBAR} get-deps compile

clean:
	${REBAR} clean skip_deps=true
	rm -f src/mysql_proto.erl
	rm -rf _build

test: deps/neotoma/ebin/neotoma.beam
	${REBAR} eunit skip_deps=true
	./covertool -cover myproto.coverdata -appname myproto -output cobertura.xml

.PHONY: test compile clean all deps
