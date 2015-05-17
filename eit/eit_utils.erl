-module(eit_utils).

-export([contains/2, join_lists/2]).
-export([to_string/3, to_string/4]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func:contains(any(), list(_)) -> boolean()
%% Description: It checks if an element is in the list or not.
%%--------------------------------------------------------------------
contains(_E, []) ->
    false;
contains(H, [H | _T]) ->
    true;
contains(Other, [_H | T]) ->
    contains(Other, T).

%%--------------------------------------------------------------------
%% Func:join_lists(list(), list()) -> list()
%% Description: It receives two lists with the same number of elements and
%% returns a new list of pairs where the first element of the pair is an
%% element of the first list and the second element is an element of the
%% second list.
%%--------------------------------------------------------------------
join_lists([], []) ->
    [];
join_lists([H1 | T1], [H2 | T2]) ->
    [{H1, H2} | join_lists(T1, T2)].

%%--------------------------------------------------------------------
%% Func:join_lists(atom(), atom(), list()) -> string()
%% Description: It returns a string which represents the function
%% 'ModuleName:FunName(Params)' where 'ModuleName', 'FunName' and 'Params' are
%% the arguments of the function.
%%--------------------------------------------------------------------
to_string(ModuleName, FunName, Params) ->
    erlang:atom_to_list(ModuleName) ++ ":" ++ erlang:atom_to_list(FunName) ++ "(" ++
        lists:foldl(
            fun(Param, Acc) ->
                Sep = case Acc of
                    "" ->
                        "";
                    _ ->
                        ", "
                end,
                Acc ++ Sep ++ to_list(Param)
            end,
            "",
            Params
        ) ++ ")".

%%--------------------------------------------------------------------
%% Func:join_lists(atom(), atom(), list(), any()) -> string()
%% Description: It returns a string which represents the function
%% 'ModuleName:FunName(Params):ReturnValue' where 'ModuleName', 'FunName',
%% 'Params' and 'ReturnValue' are the arguments of the function.
%%--------------------------------------------------------------------        
to_string(ModuleName, FunName, Params, ReturnValue) ->
    to_string(ModuleName, FunName, Params) ++ ": " ++ to_list(ReturnValue).

%%====================================================================
%% Internal functions
%%====================================================================
to_list(X)->
    case erlang:is_list(X) of
        true ->
            X;
        false ->
            case erlang:is_atom(X) of
                true ->
                    erlang:atom_to_list(X);
                false ->
                    case erlang:is_integer(X) of
                            true ->
                                erlang:integer_to_list(X);
                            false ->
                                case erlang:is_float(X) of
                                    true ->
                                        erlang:float_to_list(X);
                                    false ->
                                        "_"
                                end
                    end
            end
    end.
