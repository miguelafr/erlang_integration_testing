-module(eit).

-behaviour(gen_server).

-include("eit_data.hrl").
-record(state, {
        proxies = [],
        expects = [],
        selected_expects = [],
        errors = []
    }).

-export([start_link/0, stop/0, reset/0, verify/0,
         pre_invocation/4, post_invocation/6,
         create_proxy/1, expect/5, get_proxies/0, get_expects/0,
         delete_proxy/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================
start_link()->
    eit_conf:start_link(),
    {ok,Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

stop() ->
    eit_conf:stop(),
    gen_server:cast(?MODULE, stop).

reset() ->
    case gen_server:call(?MODULE, reset) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

verify() ->
    case gen_server:call(?MODULE, verify) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

pre_invocation(Id, ModuleName, FunName, Args)->
    case gen_server:call(?MODULE, {pre_invocation, Id, ModuleName, FunName,
            Args}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

post_invocation(Id, ModuleName, FunName, Args, Return, Ms)->
    case gen_server:call(?MODULE, {post_invocation, Id, ModuleName, FunName,
            Args, Return, Ms}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

create_proxy(ModuleName) ->
    case gen_server:call(?MODULE, {new, ModuleName}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

expect(ModuleName, FunName, Args, Return, Extra) ->
    case gen_server:call(?MODULE, {expect, ModuleName, FunName, Args,
            Return, Extra}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_proxies() ->
    case gen_server:call(?MODULE, get_proxies) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_expects() ->
    case gen_server:call(?MODULE, get_expects) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

delete_proxy(ModuleName) ->
    case gen_server:call(?MODULE, {delete_proxy, ModuleName}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

%%====================================================================
%% GenServer callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        proxies = [],
        expects = [],
        selected_expects = [],
        errors = []
    },
    {ok, State}.

handle_call(reset, _From, State) ->
    lists:foreach(
        fun({ModuleName, _ProxyMetaMod}) ->
            eit_code:remove_proxy(ModuleName)
        end,
        State#state.proxies
    ),
    NewState = #state {
        proxies = [],
        expects = [],
        selected_expects = [],
        errors = []
    },
    {reply, {ok, ok}, NewState};

handle_call(verify, _From, State) ->
    Expects = State#state.expects,
    SelectedExpects = State#state.selected_expects,

    CheckExec = lists:all(
        fun(Expect) ->
            (Expect#expect.extra)#extraTest.times == undefined
        end,
        Expects),
    
    CheckFinish = erlang:length(SelectedExpects) == 0,

    CheckErrors = erlang:length(State#state.errors) == 0,

    case {CheckExec, CheckFinish, CheckErrors} of
        {true, true, true} ->
            {reply, {ok, true}, State};
        {true, true, false} ->
            {reply, {error, State#state.errors}, State};            
        {true, false, _} ->
            {reply, {error,
                [not_finished_function] ++ State#state.errors}, State};
        {false, true, _} ->
            {reply, {error,
                [not_executed_expect] ++ State#state.errors}, State};
        {false, false, _} ->
            {reply, {error,
                [not_finished_function, not_executed_expect] ++
                    State#state.errors}, State}
    end;

handle_call({pre_invocation, Id, ModuleName, FunName, Args}, _From, State) ->
    CheckOrder = eit_conf:get_check_order(),
    GlobalCheckMatch = eit_conf:get_global_check_match(),
    case select_expect(State#state.expects,
            {ModuleName, FunName, Args}, CheckOrder, GlobalCheckMatch) of
        {no_expect_found, NewExpects} ->
            NewState = State#state {
                expects = NewExpects,
                errors = [no_expect_found | State#state.errors]
            },
            {reply, {error, no_expect_found}, NewState};
        {SelectedExpect, NewExpects} ->
            try
                check_pre_exec({ModuleName, FunName, Args}, SelectedExpect),
                NewState = State#state {
                    expects = NewExpects,
                    selected_expects = [{Id, SelectedExpect} | 
                            State#state.selected_expects]
                },
                {reply, {ok, ok}, NewState}
            catch
                _:MatchError ->
                    NewStateError = State#state {
                        errors = [MatchError | State#state.errors]
                    },
                    {reply, {error, MatchError}, NewStateError}
            end
    end;

handle_call({post_invocation, Id, ModuleName, FunName, Args, Return, Ms}, _From, State) ->
    case lists:keysearch(Id, 1, State#state.selected_expects) of
        false ->
            NewState = State#state {
                errors = [no_expect_found | State#state.errors]
            },
            {reply, {error, no_expect_found}, NewState};
        {value, {Id, SelectedExpect}} ->
            NewState = State#state {
                selected_expects = lists:keydelete(Id, 1,
                        State#state.selected_expects)
            },
            try
                check_post_exec({ModuleName, FunName, Args, Return, Ms}, SelectedExpect),
                {reply, {ok, ok}, NewState}
            catch
                _:MatchError ->
                    NewStateError = NewState#state {
                        errors = [MatchError | State#state.errors]
                    },
                    {reply, {error, MatchError}, NewStateError}
            end
    end;

handle_call({new, ModuleName}, _From, State) ->
    ProxyMetaMod = eit_code:new_proxy(ModuleName, ?MODULE),
    NewState = State#state {
        proxies = [{ModuleName, ProxyMetaMod} | State#state.proxies]
    },
    {reply, {ok, ProxyMetaMod}, NewState};

handle_call({expect, ModuleName, FunName, Args, Return, Extra}, _From, State) ->
    {ProxyMetaMod, NewState} = case lists:keysearch(ModuleName, 1, State#state.proxies) of
        false ->
            NewProxy = eit_code:new_proxy(ModuleName, ?MODULE),
            {
                NewProxy,
                State#state {
                    proxies = [{ModuleName, NewProxy} | State#state.proxies]
                }
            };
        {value, {ModuleName, Proxy}} ->
            {Proxy, State}
    end,
    NewStateExpect = NewState#state {
        expects = State#state.expects ++
            [#expect{
                module=ModuleName,
                funName = FunName,
                args = Args,
                return = Return,
                extra = Extra
            }]
    },
    {reply, {ok, ProxyMetaMod}, NewStateExpect};

handle_call(get_proxies, _From, State) ->
    {reply, {ok, State#state.proxies}, State};

handle_call(get_expects, _From, State) ->
    {reply, {ok, State#state.expects}, State};

handle_call({delete_proxy, ModuleName}, _From, State) ->
    eit_code:remove_proxy(ModuleName),
    NewState = State#state {
        proxies = lists:keydelete(ModuleName, 1, State#state.proxies)
    },
    {reply, {ok, ok}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    lists:foreach(
        fun({ModuleName, _ProxyMetaMod}) ->
            eit_code:remove_proxy(ModuleName)
        end,
        State#state.proxies
    ),
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
select_expect(Expects, {ModuleName, FunName, Args}, CheckOrder,
        GlobalCheckMatch)->
    do_select_expect(Expects, {ModuleName, FunName, Args},
        CheckOrder, GlobalCheckMatch, []).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
do_select_expect(Expects, {ModuleName, FunName, Args}, CheckOrder,
        GlobalCheckMatch, InvalidExpects)->
    case Expects of
        [Expect | MoreExpects] ->

            case CheckOrder of
                true ->
                    NewExpects = get_new_expects(
                            Expect, InvalidExpects, MoreExpects),
                    {Expect, NewExpects};

                false ->
                    case check_match_select_expect(Expect,
                            {ModuleName, FunName, Args}, GlobalCheckMatch) of
                        true ->
                            NewExpects = get_new_expects(
                                    Expect, InvalidExpects, MoreExpects),
                            {Expect, NewExpects};
                        false ->
                            do_select_expect(MoreExpects,
                                {ModuleName, FunName, Args}, CheckOrder,
                                GlobalCheckMatch, InvalidExpects ++ [Expect])
                    end
            end;
        _ ->
            {no_expect_found, Expects}
    end.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
get_new_expects(Expect, BeforeExpects, AfterExpects) ->
    ExtraTest = Expect#expect.extra,
    Times = ExtraTest#extraTest.times,
    if
        (Times == undefined) ->
            BeforeExpects ++ [Expect | AfterExpects];
        ((Times > 1) and (Times /= undefined)) ->
            NewExtra = ExtraTest#extraTest {
                times = ExtraTest#extraTest.times - 1
            },
            BeforeExpects ++ [Expect#expect {
                extra = NewExtra
             } | AfterExpects];
        true ->
            BeforeExpects ++ AfterExpects
    end.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_match_select_expect(Expect, {ModuleName, FunName, Args}, GlobalCheckMatch) ->

    % Checking module name
    case GlobalCheckMatch#checkMatch.moduleName of
        true ->
            check_expect_module_name(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    % Checking function name
    case GlobalCheckMatch#checkMatch.funName of
        true ->
            check_expect_function_name(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    % Checking number of arguments
    case GlobalCheckMatch#checkMatch.argsSize of
        true ->
            check_expect_args_size(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    % Checking argument types
    case GlobalCheckMatch#checkMatch.argsTypes of
        true ->
            check_expect_args_types(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    % Checking argument values
    case GlobalCheckMatch#checkMatch.argsValues of
        true ->
            check_expect_args_values(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    % Checking argument validations
    case GlobalCheckMatch#checkMatch.argsValidations of
        true ->
            check_expect_args_validations(Expect, {ModuleName, FunName, Args});
        false ->
            true
    end,

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_module_name(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    CheckModuleName = case Expect#expect.module of
        undefined ->
            true;
        _ ->
            Expect#expect.module == ModuleName
    end,

    return_error_match(Call, CheckModuleName, wrong_module_name,
        Expect#expect.module, ModuleName, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_function_name(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    CheckFunName = case Expect#expect.funName of
        undefined ->
            true;
        _ ->
            Expect#expect.funName == FunName
    end,

    return_error_match(Call, CheckFunName, wrong_function_name,
        Expect#expect.funName, FunName, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_args_size(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    ExtraTest = Expect#expect.extra,

    CheckArgsLength = case ExtraTest#extraTest.check_args_size of
        false ->
            true;
        _ ->
            erlang:length(Expect#expect.args) == erlang:length(Args)
    end,

    return_error_match(Call, CheckArgsLength, wrong_number_of_arguments,
        erlang:length(Expect#expect.args), erlang:length(Args), undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_args_types(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    lists:foreach(
        fun({Arg, ExpectArg}) ->
            Result = case ExpectArg#argTest.type of
                undefined ->
                    true;
                _ ->
                    erlang:apply(erlang, erlang:list_to_atom(
                            "is_" ++ erlang:atom_to_list(ExpectArg#argTest.type)),
                                [Arg])
            end,

            return_error_match(Call, Result, wrong_argument_type,
                ExpectArg#argTest.type, undefined, undefined),

            Result
        end,
        eit_utils:join_lists(Args, Expect#expect.args)),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_args_values(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    lists:foreach(
        fun({Arg, ExpectArg}) ->
            Result = not ExpectArg#argTest.check_value orelse
                    ExpectArg#argTest.value == Arg,

            return_error_match(Call, Result, wrong_argument_value,
                ExpectArg#argTest.value, Arg, undefined),

            Result
        end,
        eit_utils:join_lists(Args, Expect#expect.args)),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_args_validations(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    lists:foreach(
        fun({Arg, ExpectArg}) ->
            Result = case ExpectArg#argTest.validation of
                undefined ->
                    true;
                _ ->
                    (ExpectArg#argTest.validation)(Arg)
            end,
            return_error_match(Call, Result, wrong_validation_argument,
                undefined, undefined, undefined),

            Result
        end,
        eit_utils:join_lists(Args, Expect#expect.args)),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_extra_precondition(Expect, {ModuleName, FunName, Args})->
    Call = {ModuleName, FunName, Args},

    ExtraTest = Expect#expect.extra,

    CheckExtraPrecondition = case ExtraTest#extraTest.precondition of
        undefined ->
            true;
        _ ->
            (ExtraTest#extraTest.precondition)(ModuleName, FunName, Args)
    end,

    return_error_match(Call, CheckExtraPrecondition, wrong_extra_precondition,
        undefined, undefined, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_return_type(Expect, {ModuleName, FunName, Args, Return, _Ms})->
    Call = {ModuleName, FunName, Args, Return},

    ReturnTest = Expect#expect.return,

    CheckReturnType = case ReturnTest#returnTest.type of
        undefined ->
            true;
        _ ->
            erlang:apply(erlang, erlang:list_to_atom(
                        "is_" ++ erlang:atom_to_list(ReturnTest#returnTest.type)),
                        [Return])
    end,

    return_error_match(Call, CheckReturnType, wrong_return_type,
        ReturnTest#returnTest.type, undefined, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_return_value(Expect, {ModuleName, FunName, Args, Return, _Ms})->
    Call = {ModuleName, FunName, Args, Return},

    ReturnTest = Expect#expect.return,

    CheckReturnValue = not ReturnTest#returnTest.check_value orelse
            ReturnTest#returnTest.value == Return,

    return_error_match(Call, CheckReturnValue, wrong_return_value,
        ReturnTest#returnTest.value, Return, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_return_validation(Expect, {ModuleName, FunName, Args, Return, _Ms})->
    Call = {ModuleName, FunName, Args, Return},

    ReturnTest = Expect#expect.return,

    CheckReturnValidation = case ReturnTest#returnTest.validation of
        undefined ->
            true;
        _ ->
            (ReturnTest#returnTest.validation)(Return)
    end,
    return_error_match(Call, CheckReturnValidation, wrong_validation_return,
        undefined, undefined, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_extra_ms(Expect, {ModuleName, FunName, Args, Return, Ms})->
    Call = {ModuleName, FunName, Args, Return},

    ExtraTest = Expect#expect.extra,

    CheckExtraMs = case ExtraTest#extraTest.ms of
        undefined ->
            true;
        _ ->
            ExtraTest#extraTest.ms >= Ms
    end,
    return_error_match(Call, CheckExtraMs, timeout,
        ExtraTest#extraTest.ms, Ms, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_expect_extra_postcondition(Expect, {ModuleName, FunName, Args, Return,
            Ms})->
    Call = {ModuleName, FunName, Args, Return},

    ExtraTest = Expect#expect.extra,

    CheckExtraPostcondition = case ExtraTest#extraTest.postcondition of
        undefined ->
            true;
        _ ->
            (ExtraTest#extraTest.postcondition)
                (ModuleName, FunName, Args, Return, Ms)
    end,
    return_error_match(Call, CheckExtraPostcondition, wrong_extra_postcondition,
        undefined, undefined, undefined),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
check_pre_exec({ModuleName, FunName, Args}, Expect) ->

    % Checking module name
    check_expect_module_name(Expect, {ModuleName, FunName, Args}),

    % Checking function name
    check_expect_function_name(Expect, {ModuleName, FunName, Args}),

    % Checking number of arguments
    check_expect_args_size(Expect, {ModuleName, FunName, Args}),

    % Checking argument types
    check_expect_args_types(Expect, {ModuleName, FunName, Args}),

    % Checking argument values
    check_expect_args_values(Expect, {ModuleName, FunName, Args}),

    % Checking argument validations
    check_expect_args_validations(Expect, {ModuleName, FunName, Args}),

    % Checking extra postcondition
    check_expect_extra_precondition(Expect, {ModuleName, FunName, Args}),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------  
check_post_exec({ModuleName, FunName, Args, Return, Ms}, Expect) ->

    % Checking return type
    check_expect_return_type(Expect, {ModuleName, FunName, Args, Return, Ms}),

    % Checking return value
    check_expect_return_value(Expect, {ModuleName, FunName, Args, Return, Ms}),

    % Checking return validation
    check_expect_return_validation(Expect, {ModuleName, FunName, Args, Return,
            Ms}),

    % Checking extra milliseconds
    check_expect_extra_ms(Expect, {ModuleName, FunName, Args, Return, Ms}),

    % Checking extra postcondition
    check_expect_extra_postcondition(Expect, {ModuleName, FunName, Args, Return,
            Ms}),

    true.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
return_error_match(Call, Result, ErrorType, ExpectedValue, RealValue, Extra) ->
    if
        not Result ->
            throw({ErrorType,
                {call, Call},
                {expected, ExpectedValue},
                {real, RealValue},
                {extra, Extra}
            });
        true ->
            true
    end.
