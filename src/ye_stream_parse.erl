-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").

-record(pstate, {directives = false, cbs}).

-define(CALLBACK(NAME, STATE), case (STATE#pstate.cbs)#ye_cb_state.NAME of
        undefined -> STATE;
        Callback  -> STATE#pstate{cbs = check_cbs(Callback(STATE#pstate.cbs))}
    end).

-spec bin(binary(), #pstate{}) -> any().
bin(Binary, State) ->
    parse(Binary, #pstate{cbs = State}).

parse(<<"---\n", Rest/binary>>, State) ->
    parse(Rest, ?CALLBACK(dir_end, State));
parse(<<"...\n", Rest/binary>>, State) ->
    parse(Rest, ?CALLBACK(doc_end, State));
parse(<<$\n, Rest/binary>>, State) ->
    parse(Rest, State);
parse(<<>>, State) ->
    State#pstate.cbs.

check_cbs(#ye_cb_state{} = S) ->
    S;
check_cbs(S) ->
    erlang:error({bad_return, S}). 
