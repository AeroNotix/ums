REBAR := ./rebar3
RELX := ./relx
BUMPERL = utils/bumperl

deps:
	$(REBAR) deps

compile:
	$(REBAR) compile

console:
	@$(REBAR) shell --sname $(PROJECT_NAME)_console --config sys.config

release:
	$(REBAR) release

$(BUMPERL):
	rm -rf /tmp/bumperl/
	git clone https://github.com/aerosol/bumperl /tmp/bumperl && \
		cd /tmp/bumperl/ && \
		make deps compile escript
	cp /tmp/bumperl/bumperl $(BUMPERL)
