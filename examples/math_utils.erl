-module(math_utils).

-behaviour(gen_server).

-record(state, {
        ops = 0
    }).

-export([start_link/0, stop/0, add/2, mult/2, get_ops/0, is_started/0]).
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

add(X, Y)->
    case gen_server:call(?MODULE, {add, X, Y}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

mult(X, Y)->
    case gen_server:call(?MODULE, {mult, X, Y}) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

get_ops()->
    case gen_server:call(?MODULE, get_ops) of
        {ok, R} ->
            R;
        {error, E} ->
            throw(E)
    end.

is_started() ->
    try
        case gen_server:call(?MODULE, is_started) of
            {ok, R} ->
                R;
            {error, E} ->
                throw(E)
        end
    catch
        _ : _ ->
            false
    end.
    
%%====================================================================
%% GenServer callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        ops = 0
    },
    {ok, State}.

handle_call({add, X, Y}, _From, State) ->
    NewState = State#state {
        ops = State#state.ops + 1
    },
    {reply, {ok, X + Y}, NewState};

handle_call({mult, X, Y}, _From, State) ->
    NewState = State#state {
        ops = State#state.ops + 1
    },
    {reply, {ok, X * Y}, NewState};

handle_call(get_ops, _From, State) ->
    {reply, {ok, State#state.ops}, State};

handle_call(is_started, _From, State) ->
    {reply, {ok, true}, State};

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

