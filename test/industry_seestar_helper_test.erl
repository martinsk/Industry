-module(industry_seestar_helper_test).

-include_lib("eunit/include/eunit.hrl").

prepare_select_secondary_index_test() ->
    Keyspace = "keyspace",
    Attributes = [{id, string},
        {secondary_index, string},
        {value, string}],
    Schema = [{name, Keyspace}, {type, table}, {attributes, Attributes}],
    "SELECT id,secondary_index,value FROM keyspace.table WHERE secondary_index=secondary_index_key"
        = industry_seestar_helper:prepare_select("keyspace", table, Schema, [{secondary_index, <<"secondary_index_key">>}]).