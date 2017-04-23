REBAR := ./rebar3

all: compile

compile:
	${REBAR} compile

clean:
	${REBAR} clean skip_deps=true
	rm -f src/mysql_proto.erl
	rm -rf _build

test:
	${REBAR} eunit skip_deps=true
	./covertool -cover myproto.coverdata -appname myproto -output cobertura.xml

.PHONY: test compile clean all
