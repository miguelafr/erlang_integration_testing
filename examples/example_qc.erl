-module(example_qc).

-export([test/0]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

%%====================================================================
%% API functions
%%====================================================================
test()->
    % Initialize testing tool.
    eit:start_link(),

    % Execute quickcheck test.
    eqc:quickcheck(
        ?FORALL({A, B, X1, X2}, {int(), int(), nat(), nat()},
            ?IMPLIES(X1 =< X2,
            begin
                example:execute_test(A, B, X1, X2)
            end
            )
        )
    ),

    % Stop testing tool
    eit:stop().
