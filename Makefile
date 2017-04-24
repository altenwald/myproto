REBAR := ./rebar3

all: compile

compile:
	${REBAR} compile

clean:
	${REBAR} clean skip_deps=true
	rm -f src/mysql_proto.erl
	rm -rf _build

test:
	${REBAR} as test do xref, eunit, cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname myproto \
		-output cobertura.xml

shell:
	${REBAR} shell

.PHONY: test compile clean all shell
