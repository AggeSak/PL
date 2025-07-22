:- initialization(main).


main :-
    current_prolog_flag(argv, Argv),
    Argv = [InputFile|_],
    read_input(InputFile, Numbers),
    min_bases(Numbers, Results),
    print_results(Results),
    halt.


read_input(File, Numbers) :-
    open(File, read, Stream),
    read_line_to_string(Stream, _), % Ignore first line with count of numbers
    read_numbers(Stream, Numbers),
    close(Stream).


read_numbers(Stream, []) :-
    at_end_of_stream(Stream).
read_numbers(Stream, [Number|Numbers]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    number_string(Number, Line),
    read_numbers(Stream, Numbers).


minbases(Numbers, Results) :-
    maplist(find_base, Numbers, Results).


find_base(Number, Base) :-
    find_base_helper(Number, 2, Base).


find_base_helper(Number, Base, Base) :-
    identical_digits(Number, Base), !.
find_base_helper(Number, CurrentBase, Base) :-
    NextBase is CurrentBase + 1,
    find_base_helper(Number, NextBase, Base).


identical_digits(Number, Base) :-
    FirstDigit is Number mod Base,
    identical_digits_helper(Number, Base, FirstDigit).


identical_digits_helper(0, _, _).
identical_digits_helper(Number, Base, Digit) :-
    Digit is Number mod Base,
    NextNumber is Number // Base,
    identical_digits_helper(NextNumber, Base, Digit).

print_results([]).
print_results([Result|Results]) :-
    writeln(Result),
    print_results(Results).