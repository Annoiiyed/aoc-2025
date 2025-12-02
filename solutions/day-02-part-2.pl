:- use_module(library(pcre)).
:- use_module(library(apply)).
:- use_module(library(strings)).

append_invalid(Number, InvalidIDs, NewInvalidIDs) :-
    number_string(Number, NumStr),
    re_match("^(?P<p>\\d+?)(?P=p)+$", NumStr),
    !,
    NewInvalidIDs = [Number | InvalidIDs].

append_invalid(_, InvalidIDs, InvalidIDs).

step(Curr, Max, InvalidIDs, FinalInvalidIDs) :- 
    Curr =< Max,
    append_invalid(Curr, InvalidIDs, TempAcc),
    Next is Curr + 1,
    step(Next, Max, TempAcc, FinalInvalidIDs).

step(Curr, Max, CurrentAcc, FinalResult) :- 
    Curr > Max, 
    !, 
    FinalResult = CurrentAcc.

execute_reducer(range(Start, End), acc(InvalidIDs), acc(FinalInvalidIDs)) :-
    step(Start, End, InvalidIDs, FinalInvalidIDs).

run(Input) :-
    foldl(execute_reducer, Input, acc([]), acc(FinalInvalidIDs)),
    sum_list(FinalInvalidIDs, Sum),
    format('Part 2 answer: ~w~n', [Sum]).

parse_range(RawRange, range(Start, End)) :-
    split_string(RawRange, "-", "", [StartStr, EndStr]),
    number_string(Start, StartStr),
    number_string(End, EndStr).

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

run() :-
    read_file_to_string("day-02.txt", InputString),
    split_string(InputString, ",", "", InputListOfStrings),
    maplist(parse_range, InputListOfStrings, InputListOfRanges),
    run(InputListOfRanges),
    halt.