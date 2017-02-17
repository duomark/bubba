### Copyright (c) 2017, DuoMark International, Inc. All Rights Reserved.
### Author Jay Nelson <jay@duomark.com>

### Project name and version are defined by relx.config line:
### {release, {esse, 0.2.0}, [esse]}
PROJECT_AWK := awk '/^{release,/ {sub(/^{release, {/,       ""); sub(/,.*/,  ""); print}'
VERSION_AWK := awk '/^{release,/ {sub(/^{release, [^ ]+ "/, ""); sub(/\".*/, ""); print}'

PROJECT         := $(shell ${PROJECT_AWK} <relx.config)
PROJECT_VERSION := $(shell ${VERSION_AWK} <relx.config)
PROJECT_DESCRIPTION = Benchmarks Using Browser-Based Animation

HOST := `hostname`
ROOT := $(shell pwd)

# Set to always use 4 spaces on generated code
SP=4

# Change to V=1 for verbose debugging
V=0

DEPS = asciideck eper esse elli

dep_elli = v1.0.5
dep_esse = git https://github.com/duomark/esse 0.2.0

PLT_APPS = crypto

ERLC_OPTS := +debug_info +"{cover_enabled, true}"

include erlang.mk

CMN_EFLAGS = \
	-boot start_sasl \
	-smp enable \
	-setcookie ${COOKIE} \
	-name ${PROJECT}@${HOST} \
	+P 1000000 \
	+K true +A 160 -sbt ts

DEV_EFLAGS = \
	-pa ${ROOT}/deps/*/ebin \
	-pa ${ROOT}/ebin \
	${CMN_EFLAGS}

REL_EFLAGS = \
	-pa ${ROOT_REL}/lib/*/ebin \
	${CMN_EFLAGS}

dev:
	erl ${DEV_EFLAGS} -s bubba_app
