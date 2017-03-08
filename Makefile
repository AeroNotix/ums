PYTHON := ./.venv/bin/python
PIP := ./.venv/bin/pip
NOSE := ./.venv/bin/nosetests
REBAR := ./rebar3
RELX := ./relx
BUMPERL = utils/bumperl
BUILD_DIR = ./_build/*
ERL_DEPS_DIR = $(PWD)/$(BUILD_DIR)/lib

deps:
	$(REBAR) deps

compile:
	$(REBAR) compile

edts-console:
	@$(REBAR) shell --sname ums_console --config file/sys.config

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

define make-ct-suite
	ct_run -noshell -name test@foo.localdomain \
		    -env TEST_DIR "tests" \
		    -pa $(shell find $(ERL_DEPS_DIR) -name "*ebin*" -not -path "*priv*" -type d) \
			-sasl sasl_error_logger false \
		    -suite $(shell find tests -name "*SUITE.erl" -type f -exec echo {} \;)
endef

ct:
	$(call make-ct-suite)
