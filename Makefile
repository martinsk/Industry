PROJECT = industry
COMPILE_FIRST=industry.erl factory.erl factory_worker.erl
# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
DEPS = cowboy jiffy lager eper
include erlang.mk
