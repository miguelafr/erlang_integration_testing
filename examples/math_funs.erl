-module(math_funs).

-export([f1/3]).

f1(A, B, {X1, X2})->
    math_utils:start_link(),
    Result = lists:map(
        fun(X) ->
            C = math_utils:mult(A, X),
            math_utils:add(C, B)
        end,
        lists:seq(X1, X2)
    ),
    math_utils:stop(),
    Result.
