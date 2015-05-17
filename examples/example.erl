-module(example).

-export([test/0, execute_test/4]).

-include_lib("../eit/eit_data.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: test
%% Description: It executes a test with a fixed input data starting and
%% stopping the testing tool.
%%--------------------------------------------------------------------
test()->
    % Initialize testing tool.
    eit:start_link(),

    % Values for testing
    A = 6,
    B = 1,
    X1 = 2,
    X2 = 6,

    % Execute test
    Result = execute_test(A, B, X1, X2),

    % Stop testing tool
    eit:stop(),

    % Return the result
    Result.

%%--------------------------------------------------------------------
%% Func: execute_test
%% Description: It executes a test with the input data received as parameters.
%% The testing tool must be started to call this function. This function does
%% not stop the testing tool.
%%--------------------------------------------------------------------
execute_test(A, B, X1, X2)->

    % Do not pring debug information.
    eit_conf:set_debug(false),

    % Check that functions are called in order.
    eit_conf:set_check_order(true),

    % If a test fails, the program continues its execution.
    eit_conf:set_exit_on_error(false),

    % This function calls must not be taken into account.
    eit_conf:set_ignore_checking([{math_utils, handle_call},
                              {math_utils, handle_cast},
                              {math_utils, terminate},
                              {math_utils, init},
                              {math_utils, start_link},
                              {math_utils, is_started},
                              {math_utils, get_ops},
                              {math_utils, stop}]),

    % Expected integration
    lists:foreach(
        fun(I) ->
            expect_mult(A, B, X1, X2, (I - X1)),
            expect_add(A, B, X1, X2, (I - X1))
        end,
        lists:seq(X1, X2)),

    % Execute program
    math_funs:f1(A, B, {X1, X2}),

    % Verify not executed expects or not terminated executions
    V = try
        eit:verify()
    catch
        E ->
            io:format("~p ~n", [E]),
            false
    end,

    % Prepare the testing tool for a new execution
    eit:reset(),

    V.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: expect_mult
%% Description: It creates an 'expect' for the operation 'math_utils:mult'.
%%--------------------------------------------------------------------
expect_mult(A, B, X1, X2, I) ->
    eit:expect(math_utils, mult,
        [
            #argTest {
                type=integer, % Type of the first argument
                value = A, % Value of the first argument
                check_value = true % Check the value of the first argument
            },
            #argTest{
                type=integer, % Type of the second argument
                validation = % Validation function for the second argument
                    fun(Arg) ->
                        Arg >= X1 andalso Arg =< X2
                    end
            }
        ],
        #returnTest{
            type = integer % Type of the return value
        },
        #extraTest {
            precondition = % Function that must be true before executing
                           % 'math_utils:mult'
                fun(_Mod, _Fun, _Args) ->
                    math_utils:is_started()
                end,
            postcondition = % Function that must be true after executing 
                            % 'math_utils:mult'
                fun(_Mod, _Fun, _Args, _Return, _Ms) ->
                    math_utils:get_ops() == I * 2 + 1
                end,
            times = 1, % How many times this function must be executed
            ms = 1000 % Maximum response time of 'math_utils:mult'
        }
    ).

%%--------------------------------------------------------------------
%% Func: expect_mult
%% Description: It creates an 'expect' for the operation 'math_utils:add'.
%%--------------------------------------------------------------------
expect_add(A, B, X1, X2, I) ->
    eit:expect(math_utils, add,
        [
            #argTest {
                type=integer % Type of the first argument
            },
            #argTest{
                type=integer, % Type of the second argument
                value = B, % Value of the second argument
                check_value = true % Check the value of the second argument
            }
        ],
        #returnTest{
            type=integer % Type of the return value
        },
        #extraTest {
            precondition = % Function that must be true before executing
                           % 'math_utils:add'
                fun(_Mod, _Fun, _Args) ->
                    math_utils:is_started()
                end,
            postcondition = % Function that must be true after executing
                            % 'math_utils:add'
                fun(_Mod, _Fun, _Args, _Return, _Ms) ->
                    math_utils:get_ops() == I * 2 + 2
                end,
            times = 1, % How many times this function must be executed
            ms = 1000 % Maximum response time of 'math_utils:add'
        }
    ).
