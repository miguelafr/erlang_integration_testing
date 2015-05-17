-module(eit_code).

-export([new_proxy/2, remove_proxy/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func:new_proxy(atom(), atom()) -> ProxyMetaMod
%% Description: It creates a new mock proxy for the module ModuleName using the
%% smerl utilities. This proxy calls CallbackModuleName:pre_invocation
%% before executing each function and CallbackModuleName:post_invocation after
%% executing each function.
%%--------------------------------------------------------------------
new_proxy(ModuleName, CallbackModuleName)->
    NewModuleName = get_proxy_module_name(ModuleName),
    {ok, MetaMod} = smerl:for_module(ModuleName),

    ProxyMetaMod = lists:foldl(
        fun({FunName, Arity}, AuxProxyMetaMod) ->
            Params = if
                Arity > 0 ->
                    lists:foldl(
                        fun(N, Acc) ->
                            Sep = case Acc of
                                "" ->
                                    "";
                                _ ->
                                    ", "
                            end,
                            Acc ++ Sep ++ "V" ++ erlang:integer_to_list(N)
                        end,
                        "",
                        lists:seq(1, Arity)
                    );
                true ->
                    ""
            end,

            Impl = erlang:atom_to_list(FunName) ++ "(" ++ Params ++ ")" ++ "-> "
                ++ "try "

                ++     print_debug(ModuleName, FunName, Params) ++ ", "

                ++     "Id = erlang:integer_to_list("
                ++             "calendar:datetime_to_gregorian_seconds("
                ++                 "calendar:now_to_datetime(erlang:now())"
                ++             ") + random:uniform(10000)"
                ++          "),"

                ++     print_invocation(ModuleName, FunName, CallbackModuleName,
                           "pre_invocation",
                           "Id, " ++ erlang:atom_to_list(ModuleName) ++ ", "
                              ++ erlang:atom_to_list(FunName) ++ ", "
                              ++ "[" ++ Params ++ "] ",
                           "preconditions", 
                           erlang:atom_to_list(ModuleName) ++ ", "
                               ++ erlang:atom_to_list(FunName) ++ ", " 
                               ++ "[" ++ Params ++ "]"
                       ) ++ ", "

                ++     "{Time, Value} = timer:tc("
                ++          erlang:atom_to_list(NewModuleName) ++ ", "
                ++          erlang:atom_to_list(FunName) ++ ", " 
                ++          "[" ++ Params ++ "]), "

                ++     print_invocation(ModuleName, FunName, CallbackModuleName,
                           "post_invocation",
                           "Id, " ++ erlang:atom_to_list(ModuleName) ++ ", "
                              ++ erlang:atom_to_list(FunName) ++ ", "
                              ++ "[" ++ Params ++ "] " ++ ", "
                              ++ "Value" ++ ", "
                              ++ "Time",
                           "postconditions", 
                           erlang:atom_to_list(ModuleName) ++ ", "
                               ++ erlang:atom_to_list(FunName) ++ ", " 
                               ++ "[" ++ Params ++ "]" ++ ", " 
                               ++ "Value"
                       ) ++ ", "

                ++     "Id, Time, " % This line is added to avoid warnings on compile
                ++     "Value "

                ++ "catch "
                ++     "_:E -> io:format(\"ERROR: ~p ~n\", [E]), "
                ++          "throw(E)"
                ++ "end.",

            {ok, NewProxyMetaMod} = smerl:add_func(AuxProxyMetaMod, Impl),
            NewProxyMetaMod
        end,
        smerl:new(ModuleName),
        smerl:get_exports(MetaMod)
    ),

    smerl:compile(ProxyMetaMod),
    change_module_name(ModuleName, NewModuleName),
    ProxyMetaMod.

%%--------------------------------------------------------------------
%% Func:remove_proxy(atom()) -> ok
%% Description: It unload the module ModuleName from the memory.
%%--------------------------------------------------------------------    
remove_proxy(ModuleName)->
    NewModuleName = get_proxy_module_name(ModuleName),
    code:purge(NewModuleName),
    code:delete(NewModuleName),
    code:purge(ModuleName),
    code:delete(ModuleName). 

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func:change_module_name(atom(), atom()) -> ok
%% Description: It changes the name of the module Module to NewModuleName using
%% the smerl utilities.
%%--------------------------------------------------------------------    
change_module_name(Module, NewModuleName) ->
    {ok, MetaMod} = smerl:for_module(Module),
    NewMetaMod = smerl:set_module(MetaMod, NewModuleName),
    smerl:compile(NewMetaMod).

%%--------------------------------------------------------------------
%% Func:get_proxy_module_name(atom()) -> atom()
%% Description: It generates a new name for the module ModuleName that can be
%% used when the original module is going to be replaced with a mock proxy
%% module.
%%--------------------------------------------------------------------
get_proxy_module_name(ModuleName) ->
    erlang:list_to_atom(erlang:atom_to_list(ModuleName) ++ "_orig").

%get_module_name(MetaMod) ->
%    erlang:element(2, MetaMod).

%%--------------------------------------------------------------------
%% Func:print_invocation(atom(), atom(), atom(), string(), string(),
%%                       string(), string()) -> string()
%% Description: It generates a string with source code.
%%--------------------------------------------------------------------
print_invocation(ModuleName, FunName, CallbackModuleName,
        Invocation, InvocationParams,
        InvocationType,  ToStringParams) ->

    IgnoreChecking = eit_conf:get_ignore_checking(),
    Debug = eit_conf:get_debug(),

    case eit_utils:contains({ModuleName, FunName}, IgnoreChecking) of
        false ->
            ExitOnError = eit_conf:get_exit_on_error(),
            ErrVar = "Err" ++ InvocationType,

            PrintDebug =
                case Debug of
                    true ->
                        "io:format(\"ERROR checking " ++ InvocationType
                        ++    " for function <~s>: ~p ~n\", "
                        ++    "[eit_utils:to_string(" ++ ToStringParams ++ "), "
                        ++    ErrVar ++ "])";
                    false ->
                        "ok"
                end,

               "try "
            ++     erlang:atom_to_list(CallbackModuleName) ++ ":"
            ++         Invocation ++ "(" ++ InvocationParams ++ ") "
            ++ "catch "
            ++     "_:" ++ ErrVar ++ "-> "
            ++         PrintDebug ++ ", "
            ++         "case " ++ erlang:atom_to_list(ExitOnError) ++ " of "
            ++             "true -> "
            ++                 "exit(" ++ ErrVar ++ "); "
            ++             "false -> "
            ++                 "ok "
            ++          "end "
            ++ "end";

        true ->
            "ok"
    end.

%%--------------------------------------------------------------------
%% Func:print_debug(atom(), atom(), list()) -> string()
%% Description: It generates a string with source code.
%%--------------------------------------------------------------------
print_debug(ModuleName, FunName, Params)->
    Debug = eit_conf:get_debug(),

    case Debug of
        true ->
            "io:format(\"*** Executing: ~s~n\", [eit_utils:to_string("
            ++ erlang:atom_to_list(ModuleName) ++ ", "
            ++ erlang:atom_to_list(FunName) ++ ", " 
            ++ "[" ++ Params ++ "])])";
        false ->
            "ok"
    end.
