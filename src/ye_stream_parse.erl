-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").


-spec bin(binary(), #ye_cb_state{}) -> any().

bin(<<"---", Rest/binary>>, State) ->
        bin(Rest, (State#ye_cb_state.doc_begin)(State));
bin(<<"...", Rest/binary>>, State) ->
        bin(Rest, (State#ye_cb_state.doc_end)(State));
bin(<<>>, State) ->
        State#ye_cb_state.state.
