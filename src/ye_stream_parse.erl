-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").

-record(pstate, {directives = false, cbs}).

-define(CALLBACK(NAME, ARGS, STATE), 
    (if 
        ((STATE#pstate.cbs)#ye_cb_state.NAME =:= undefined) -> STATE;
        true -> STATE#pstate{cbs =
                check_cbs(apply((STATE#pstate.cbs)#ye_cb_state.NAME,
                        [STATE#pstate.cbs | ARGS]))}
    end)).
-define(IS_WHITESPACE(C), C =:= $\ ; C=:= $\n; C =:= $\t).

-spec bin(binary(), #pstate{}) -> any().
bin(Binary, State) ->
    parse(Binary, #pstate{cbs = State}).

parse(<<"---", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    X = ?CALLBACK(directives_end, [], State),
    parse(Rest, ?CALLBACK(doc_begin, [], X));
parse(<<"...", C, Rest/binary>>, State) when ?IS_WHITESPACE(C) ->
    parse(Rest, ?CALLBACK(doc_end, [], State));
parse(<<$#, Rest/binary>>, State) ->
    {Comment, Rest2} = consume_until_newline(Rest),
    parse(Rest2, ?CALLBACK(comment, [Comment], State));
parse(<<$\n, Rest/binary>>, State) ->
    parse(Rest, State);
parse(<<>>, State) ->
    State#pstate.cbs;
parse(<<Unknown, _Rest/binary>>, _State) ->
    erlang:error({yaml_syntax_error, Unknown}).

consume_until_newline(Bin) ->
    consume_until_newline(Bin, <<>>).
consume_until_newline(<<$\n, Bin/binary>>, Acc) ->
    {Acc, Bin};
consume_until_newline(<<C, Bin/binary>>, Acc) ->
    consume_until_newline(Bin, <<Acc/binary, C>>).

check_cbs(#ye_cb_state{} = S) ->
    S;
check_cbs(S) ->
    erlang:error({bad_return, S}). 
