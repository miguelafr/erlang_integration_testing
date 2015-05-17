-module(eit_conf).

-behaviour(gen_server).

-include("eit_data.hrl").
-record(state, {
        check_order = false,
        global_check_match,
        exit_on_error = false,
        ignore_checking = [],
        debug = false
    }).

-export([start_link/0, stop/0,
         get_check_order/0, set_check_order/1,
         get_exit_on_error/0, set_exit_on_error/1,
         get_ignore_checking/0, set_ignore_checking/1,
         get_global_check_match/0, set_global_check_match/1,
         get_debug/0, set_debug/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% API functions
%%====================================================================
start_link()->
    {ok,Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

stop() ->
    gen_server:cast(?MODULE, stop).

get_check_order() ->
    case gen_server:call(?MODULE, get_check_order) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

set_check_order(CheckOrder) ->
    case gen_server:call(?MODULE, {set_check_order, CheckOrder}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_exit_on_error() ->
    case gen_server:call(?MODULE, get_exit_on_error) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

set_exit_on_error(ExitOnError) ->
    case gen_server:call(?MODULE, {set_exit_on_error, ExitOnError}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_ignore_checking() ->
    case gen_server:call(?MODULE, get_ignore_checking) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

set_ignore_checking(IgnoreChecking) ->
    case gen_server:call(?MODULE, {set_ignore_checking, IgnoreChecking}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_global_check_match() ->
    case gen_server:call(?MODULE, get_global_check_match) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

set_global_check_match(GlobalCheckMatch) ->
    case gen_server:call(?MODULE, {set_global_check_match, GlobalCheckMatch}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_debug() ->
    case gen_server:call(?MODULE, get_debug) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

set_debug(Debug) ->
    case gen_server:call(?MODULE, {set_debug, Debug}) of
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
        check_order = false,
        global_check_match = #checkMatch {},
        exit_on_error = false,
        ignore_checking = []
    },
    {ok, State}.

handle_call(get_check_order, _From, State) ->
    {reply, {ok, State#state.check_order}, State};

handle_call({set_check_order, CheckOrder}, _From, State) ->
    NewState = State#state {
        check_order = CheckOrder
    },
    {reply, {ok, ok}, NewState};

handle_call(get_ignore_checking, _From, State) ->
    {reply, {ok, State#state.ignore_checking}, State};

handle_call({set_ignore_checking, IgnoreChecking}, _From, State) ->
    NewState = State#state {
        ignore_checking = IgnoreChecking
    },
    {reply, {ok, ok}, NewState};

handle_call(get_exit_on_error, _From, State) ->
    {reply, {ok, State#state.exit_on_error}, State};

handle_call({set_exit_on_error, ExitOnError}, _From, State) ->
    NewState = State#state {
        exit_on_error = ExitOnError
    },
    {reply, {ok, ok}, NewState};

handle_call(get_global_check_match, _From, State) ->
    {reply, {ok, State#state.global_check_match}, State};

handle_call({set_global_check_match, GlobalCheckMatch}, _From, State) ->
    NewState = State#state {
        global_check_match = GlobalCheckMatch
    },
    {reply, {ok, ok}, NewState};

handle_call(get_debug, _From, State) ->
    {reply, {ok, State#state.debug}, State};

handle_call({set_debug, Debug}, _From, State) ->
    NewState = State#state {
        debug = Debug
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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
