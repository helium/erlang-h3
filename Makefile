.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) as test do ct

typecheck:
	$(REBAR) dialyzer

doc:
	$(REBAR) edoc

ci:
	$(REBAR) dialyzer
	$(REBAR) as test do ct
	$(REBAR) as test do cover
	$(REBAR) covertool generate
	codecov -f _build/test/covertool/h3.covertool.xml
