PYTHON := ./.venv/bin/python
PIP := ./.venv/bin/pip
NOSE := ./.venv/bin/nosetests
REBAR := ./rebar3
RELX := ./relx
BUMPERL = utils/bumperl
BUILD_DIR = ./_build/*
ERL_DEPS_DIR = $(PWD)/$(BUILD_DIR)/lib
GCLOUD_PROJECT?=
DOCKER_IMAGE_NAME=ums
DOCKER_IMAGE_TAG=$(shell git rev-parse --short HEAD)
export


docker-build:
	docker build \
	--squash \
	-t gcr.io/${GCLOUD_PROJECT}/${DOCKER_IMAGE_NAME}:${DOCKER_IMAGE_TAG} .

docker-push:
	@gcloud docker -- push gcr.io/${GCLOUD_PROJECT}/${DOCKER_IMAGE_NAME}:${DOCKER_IMAGE_TAG}

deploy:
	envsubst < "kubernetes/deployment.yml" | kubectl apply -f -

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

run:
	./_build/default/rel/ums/bin/ums console

test: python-tests ct

python-tests:
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

docker-console:
	/usr/app/_build/default/rel/ums/bin/ums foreground
