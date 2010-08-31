-module(ye_stream_parse).
-export([bin/2]).

-include("yeml.hrl").

-record(pstate, {
		bin,
		lvl = 0,
		in_node = false,
		line = 1,
		cbs
	}).

-define(CALLBACK(NAME, ARGS, STATE), 
    (if 
        (((STATE)#pstate.cbs)#ye_cb_state.NAME =:= undefined) -> (STATE);
        true -> (STATE)#pstate{cbs =
                check_cbs(apply(((STATE)#pstate.cbs)#ye_cb_state.NAME,
                        [(STATE)#pstate.cbs | ARGS]))}
    end)).
-define(IS_WHITESPACE(C), C =:= $\ ; C=:= $\n; C =:= $\t).
-define(IS_NB_WHITESPACE(C), C =:= $\ ; C =:= $\t).
-define(IS_NOT_WHITESPACE(C), C =/= $\ , C =/= $\t, C=/= $\n).

-spec bin(binary(), #ye_cb_state{}) -> any().
bin(Binary, State) ->
    try 
        root(#pstate{bin = Binary, cbs = State})
    catch 
        error:function_clause ->
            [{Module, Function, [StackState]}|_] = erlang:get_stacktrace(),
            {parse_error, {{line, StackState#pstate.line},
                    {module, Module}, {function, Function}}};
		_:{end_of_stream, State} ->
			State#pstate.cbs
    end.

root(#pstate{bin = <<>>, cbs = CallbackState}) ->
    CallbackState;
root(#pstate{bin = <<C, Rest/binary>>} = State) when ?IS_WHITESPACE(C) ->
	root(parse_root(strip_empty_lines(State#pstate{bin = <<C, Rest/binary>>})));
root(State) ->
	root(parse_root(State)).

parse_root(#pstate{bin = <<"---">>} = State) ->
	NextState = State#pstate{bin = <<>>},
    X = ?CALLBACK(directives_end, [], NextState),
    ?CALLBACK(doc_begin, [], X);
parse_root(#pstate{bin = <<"---", C, Rest/binary>>} = State) when ?IS_WHITESPACE(C) ->
	NextState = State#pstate{bin = <<C, Rest/binary>>},
	X = ?CALLBACK(directives_end, [], NextState),
    ?CALLBACK(doc_begin, [], X);
parse_root(#pstate{bin = <<"...", C, Rest/binary>>} = State) when ?IS_WHITESPACE(C) ->
	NextState = State#pstate{bin = <<C, Rest/binary>>},
    ?CALLBACK(doc_end, [], NextState);
parse_root(State) ->
    generic(State).

generic(#pstate{bin = <<>>} = State) ->
	State;
generic(#pstate{lvl = PrevLvl} = State) ->
    NextState = case next_token(State) of
		{block_end, S} ->
			S;
        {scalar, Scalar, S} ->
            ?CALLBACK(scalar, [Scalar], S);
        {sequence, S} ->
			X = generic(?CALLBACK(seq_begin, [], S)),
            Y = ?CALLBACK(seq_end, [], X),
			Y#pstate{lvl = PrevLvl};
        {mapping, Key, S} ->
			X = generic(?CALLBACK(mapping_begin, [Key], S)),
			X#pstate{lvl = PrevLvl};
		{comment, Comment, S} ->
			?CALLBACK(comment, [Comment], S)
    end,
    generic(strip_empty_lines(NextState)).

next_token(#pstate{bin = <<$\n, Rest>>, lvl = PrevLvl} = State) ->
    case indentation_level(State#pstate{bin = Rest}) of
		#pstate{lvl = Level} when Level < PrevLvl ->
			{block_end, State};
		#pstate{lvl = Level} = S when Level =:= PrevLvl ->
			find_next_token(S)
	    % Heh, we should never? end up here
    end;
next_token(State) ->
	find_next_token(State).

find_next_token(#pstate{bin = <<$-, C, Rest/binary>>} = S) when ?IS_WHITESPACE(C) ->
	Lvl = S#pstate.lvl + 1,
	{sequence, S#pstate{bin = Rest, lvl = Lvl, in_node = true}};
find_next_token(#pstate{bin = <<$#, Rest/binary>>} = S) ->
	{Comment, NextState} = consume_until_newline(S#pstate{bin = Rest}),
	{comment, Comment, NextState};
find_next_token(#pstate{bin = <<C, _/binary>>} = S) when ?IS_NOT_WHITESPACE(C) ->
	parse_scalar(S);
find_next_token(#pstate{bin = <<_, Rest/binary>>} = S) ->
	find_next_token(S#pstate{bin = Rest}).

parse_scalar(State) ->
    parse_scalar(State, <<>>).

parse_scalar(#pstate{bin = <<$:, Rest/binary>>} = State, Acc) ->
    {mapping, Acc, State#pstate{bin = Rest, in_node = true}};
parse_scalar(#pstate{bin = <<$\n, Rest/binary>>, lvl = PrevLevel} = State, Acc) ->
	PrevLvl = State#pstate.lvl,
	case indentation_level(State#pstate{bin = Rest}) of
		#pstate{lvl = Level} = S when Level >= PrevLevel ->
			Line = State#pstate.line + 1,
			parse_scalar(S#pstate{bin = Rest, line = Line}, Acc);
		#pstate{lvl = Level} when Level < PrevLvl ->
			{scalar, Acc, State#pstate{bin = Rest, in_node = false}}
	end;
parse_scalar(#pstate{bin = <<$ , $#, Rest/binary>>} = State, Acc) ->
	{scalar, Acc, State#pstate{bin = Rest, in_node = true}};
parse_scalar(#pstate{bin = <<C, Rest/binary>>} = S, Acc) ->
	parse_scalar(S#pstate{bin = Rest}, <<Acc/binary, C>>);
parse_scalar(#pstate{bin = <<>>} = S, Acc) ->
	throw({end_of_stream, S#pstate{bin = Acc}}).

consume_until_newline(State) ->
    consume_until_newline(State, <<>>).
consume_until_newline(#pstate{bin = <<>>} = State, Acc) ->
    {Acc, State};
consume_until_newline(#pstate{bin = <<$\n, Bin/binary>>} = State, Acc) ->
	{Acc, State#pstate{bin = Bin}};
consume_until_newline(#pstate{bin = <<C, Bin/binary>>} = State, Acc) ->
    consume_until_newline(State#pstate{bin = Bin}, <<Acc/binary, C>>).

strip_empty_lines(#pstate{bin = Bin} = State) ->
    strip_empty_lines(State, Bin).

strip_empty_lines(#pstate{bin = <<C, Rest/binary>>} = State, Bin) when ?IS_NB_WHITESPACE(C) ->
    strip_empty_lines(State#pstate{bin = Rest}, Bin);
strip_empty_lines(#pstate{bin = <<$\n, Rest/binary>>} = State, _) ->
    strip_empty_lines(State#pstate{bin = Rest, line = State#pstate.line + 1}, Rest);
strip_empty_lines(State, Bin) ->
    State#pstate{bin = Bin}.

indentation_level(State) ->
    indentation_level(State, 0).

indentation_level(#pstate{bin = <<$\ , Rest/binary>>} = State, Lvl) ->
	indentation_level(State#pstate{bin = Rest}, Lvl + 1);
indentation_level(State, Lvl) ->
    State#pstate{lvl = Lvl, in_node = true}.

check_cbs(#ye_cb_state{} = S) ->
    S;
check_cbs(S) ->
    erlang:error({bad_return, S}). 
