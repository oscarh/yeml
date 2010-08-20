-module(yaml_test).

-include_lib("eunit/include/eunit.hrl").
-include("yeml.hrl").

-define(TEST_FILE, "../test/test.yaml").

parse_test() ->
        {ok, Bin} = file:read_file(?TEST_FILE),
        State = #ye_cb_state{
                doc_begin = fun(S) -> io:format("Document begin"), S end,
                doc_end = fun(S) -> io:format("Document end"), S end
        },
        ?_assertMatch(ok, check_result(ye_stream_parse:bin(Bin, State))).

check_result(_) ->
        ok.
