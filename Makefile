PROJECT = industry
COMPILE_FIRST=industry.erl factory.erl factory_worker.erl
# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

DEPS = cowboy jiffy lager eper rafter seestar

dep_rafter = git https://github.com/martinsk/rafter master
dep_seestar = git https://github.com/iamaleksey/seestar  master

include erlang.mk
