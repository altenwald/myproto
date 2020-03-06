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
	${REBAR} do xref, eunit, cover, covertool generate
	mv _build/test/covertool/myproto.covertool.xml cobertura.xml

shell:
	${REBAR} shell

.PHONY: test compile clean all shell doc
