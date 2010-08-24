-module(yaml_test).

-include_lib("eunit/include/eunit.hrl").
-include("yeml.hrl").

-define(SIMPLE_FILE, "../test/simple.yaml").

simlpe_document_test() ->
    {ok, Bin} = file:read_file(?SIMPLE_FILE),
    State = #ye_cb_state{
        state= [doc_begin, doc_end, {comment, <<"End document">>}],
        doc_begin = fun(S) ->
                ?assertMatch(doc_begin, hd(S#ye_cb_state.state)),
                S#ye_cb_state{state=tl(S#ye_cb_state.state)}
            end,
        doc_end = fun(S) ->
                ?assertMatch(doc_end, hd(S#ye_cb_state.state)),
                S#ye_cb_state{state = tl(S#ye_cb_state.state)}
            end,
        comment = fun(S, Comment) ->
                ?assertMatch({comment, Comment}, hd(S#ye_cb_state.state)),
                S#ye_cb_state{state=tl(S#ye_cb_state.state)}
            end
    },
    check_result(ye_stream_parse:bin(Bin, State)).

check_result(State) ->
    ?_assertMatch(State, []).
