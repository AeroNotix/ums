PYTHON := ./.venv/bin/python
PIP := ./.venv/bin/pip
NOSE := ./.venv/bin/nosetests
REBAR := ./rebar3
RELX := ./relx
BUMPERL = utils/bumperl

deps:
	$(REBAR) deps

compile:
	$(REBAR) compile

console:
	@$(REBAR) shell --sname $(PROJECT_NAME)_console --config sys.config

${PIP}:
	virtualenv -p "$$(which python3)" .venv

python-deps: ${PIP}
	${PIP} install -r requirements.txt

release:
	$(REBAR) release

test:
	${NOSE} tests

$(BUMPERL):
	rm -rf /tmp/bumperl/
	git clone https://github.com/aerosol/bumperl /tmp/bumperl && \
		cd /tmp/bumperl/ && \
		make deps compile escript
	cp /tmp/bumperl/bumperl $(BUMPERL)
