:- use_module(library(apply)).
:- use_module(library(strings)).

add_pass_count(Count, 0, NewCount) :- NewCount is Count + 1.
add_pass_count(Count, _, Count) :- !.

add_endsinzero_count(Count, 0, NewCount) :- NewCount is Count + 1.
add_endsinzero_count(Count, _, Count) :- !.

clock_step(_, Pos, 0, Passes, Pos, Passes) :- !.

clock_step("R", Pos, Steps, Passes, FinalPos, FinalPasses) :-
    Steps > 0,
    NextPosRaw is Pos + 1,
    NextPos is NextPosRaw mod 100,
    add_pass_count(Passes, NextPos, NextPasses),
    NextSteps is Steps - 1,
    clock_step("R", NextPos, NextSteps, NextPasses, FinalPos, FinalPasses).

clock_step("L", Pos, Steps, Passes, FinalPos, FinalPasses) :-
    Steps > 0,
    NextPosRaw is Pos - 1,
    NextPos is NextPosRaw mod 100,
    add_pass_count(Passes, NextPos, NextPasses),
    NextSteps is Steps - 1,
    clock_step("L", NextPos, NextSteps, NextPasses, FinalPos, FinalPasses).

execute_reducer(RawInstruction, acc(OldPos, OldEIZ, OldZeroPasses), acc(NewPos, NewEIZ, NewZeroPasses)) :-
    parse_instruction(RawInstruction, instruction(Direction, Number)),
    clock_step(Direction, OldPos, Number, OldZeroPasses, NewPos, NewZeroPasses),
    add_endsinzero_count(OldEIZ, NewPos, NewEIZ).

parse_instruction(InputString, instruction(Direction, Number)) :-
    sub_string(InputString, 0, 1, _, Direction),
    sub_string(InputString, 1, _, 0, NumStr),
    number_string(Number, NumStr).

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

run(Input) :-
    string_lines(Input, List),
    % Start the reduction with initial state: Position 50, EndsInZeroCount 0, ZeroPasses 0
    InitialAcc = acc(50, 0, 0), 

    foldl(execute_reducer, List, InitialAcc, FinalAcc),
        
    (FinalAcc = acc(FinalPos, FinalEndsInZeroCount, FinalZeroPasses) ->
        format('Position: ~w~n', [FinalPos]),
        format('Part 1 answer: ~w~n', [FinalEndsInZeroCount]),
        format('Part 2 answer: ~w~n', [FinalZeroPasses])
    ; true).
    
run() :-
    read_file_to_string("day-01.txt", Input),
    run(Input),
    halt.