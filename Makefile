PYTHON := ./.venv/bin/python2
PIP := ./.venv/bin/pip
NOSE := ./.venv/bin/nosetests
REBAR := ./rebar3
RELX := ./relx
BUMPERL = utils/bumperl
BUILD_DIR = ./_build/*
ERL_DEPS_DIR = $(PWD)/$(BUILD_DIR)/lib
REL_DIR=./_build/dev/rel/ums
DOCKER_IMAGE_NAME=ums
DOCKER_IMAGE_TAG=$(shell git rev-parse --short HEAD)
export

all: release test

docker-build-dependencies:
	@docker build -t ums-dependencies -f Dockerfile.dependencies .

docker-build:
	@docker build -t ${DOCKER_IMAGE_NAME}-${DOCKER_IMAGE_TAG} .

deps:
	$(REBAR) as dev deps

compile:
	$(REBAR) as dev compile

edts-console:
	@$(REBAR) shell --sname ums_console --config file/sys.config

${PIP}:
	virtualenv -p "$$(which python)" .venv

python-deps: ${PIP}
	@${PIP} install -q -r requirements.txt

python-tests:
	${NOSE} tests

release:
	@$(REBAR) as dev release

release-console:
	@${REL_DIR}/bin/ums console

release-start:
	@${REL_DIR}/bin/ums start
	@while [ `curl -s -o /dev/null -w "%{http_code}" localhost:5564/health` -eq "000" ]; do \
		echo "Waiting for release..."; sleep 3; \
	done;

release-attach:
	${REL_DIR}/bin/ums attach

release-stop:
	@-$(REL_DIR)/bin/ums stop 2>&1 > /dev/null || true
	@-pkill -15 -f "$$(pwd)/$(REL_DIR)/erts-5.10.1/bin/run_erl" 2>&1 > /dev/null || true
	@-while ps -ef | grep "/_rel -pro[g]name ums" > /dev/null; do sleep 1; done

skaffold:
	curl -Lo skaffold https://storage.googleapis.com/skaffold/releases/latest/skaffold-linux-amd64 && chmod +x skaffold

test: python-tests ct

$(BUMPERL):
	rm -rf /tmp/bumperl/
	git clone https://github.com/aerosol/bumperl /tmp/bumperl && \
		cd /tmp/bumperl/ && \
		make deps compile escript
	cp /tmp/bumperl/bumperl $(BUMPERL)

define make-ct-suite
	@ct_run -noshell -name test@foo.localdomain \
		    -env TEST_DIR "tests" \
		    -pa $(shell find $(ERL_DEPS_DIR) -name "*ebin*" -not -path "*priv*" -type d) \
			-sasl sasl_error_logger false \
		    -suite $(shell find tests -name "*SUITE.erl" -type f -exec echo {} \;)
endef

ct: release-stop
	$(call make-ct-suite)

docker-console:
	/usr/app/_build/default/rel/ums/bin/ums foreground
