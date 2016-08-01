PROJECT = edr
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)

.PHONY: all compile clean

all: compile

BACKEND_MODULES = $(shell ls src/modules/*.erl | sed 's/src\/modules\///;s/\.erl/,/' | sed '$$s/.$$//')
APP_MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(BACKEND_MODULES),$(APP_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/modules/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump
