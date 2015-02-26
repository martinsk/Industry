ERL_LIBS=$(pwd) erl +P 2000000 -pa deps/*/deps/*/ebin -pa deps/*/ebin -pa ebin -boot start_sasl -s lager -s inets -s crypto -s ssl \
    -name industry@$(hostname) \
    -setcookie industries
