REBAR := ./rebar3

all: compile

doc:
	${REBAR} edown

compile:
	${REBAR} compile

clean:
	${REBAR} clean
	rm -rf _build

test:
	${REBAR} do xref, eunit, cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname myproto \
		-output cobertura.xml

shell:
	${REBAR} shell

.PHONY: test compile clean all shell doc
