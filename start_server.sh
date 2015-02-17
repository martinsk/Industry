ERL_LIBS=$(pwd) erl i-P 1000000 -pa deps/*/ebin -pa ebin -boot start_sasl -s lager -s inets -s crypto -s ssl \
    -name industry@$(hostname) \
    -setcookie industries
