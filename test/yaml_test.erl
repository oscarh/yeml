-module(yaml_test).

-include_lib("eunit/include/eunit.hrl").
-include("yeml.hrl").

-define(TEST_FILE, "../test/test.yaml").

parse_test() ->
    {ok, Bin} = file:read_file(?TEST_FILE),
    State = #ye_cb_state{
        state= [doc_begin, doc_end, {comment, "End document"}],
        doc_begin = fun(S) -> [doc_begin | T] = S#ye_cb_state.state,
                S#ye_cb_state{state=T} end,
        doc_end = fun(S) -> [doc_end | T] = S#ye_cb_state.state,
                S#ye_cb_state{state=T} end,
        comment = fun(S, Comment) -> [{comment, Comment} | T] =
                S#ye_cb_state.state,
                S#ye_cb_state{state=T} end
    },
    check_result(ye_stream_parse:bin(Bin, State)).

check_result(State) ->
    ?_assertMatch(State, []).
