% nat lang input tester
/*
% Objective, 
% prompt user to type a phrase
% tokenize phrase into list of words (list split/remove certain characters)
% parse list using defined rules (...) and create a 'command' out of it
% execute said command and go thru motions of updating game normally
% repeat



How I imagine our natural language rules work:
legal verb + legal nounphrase (go north, move north, take sword)
or
legal verb (bag, fight (if only 1 enemy around), reset... )?

also take out determiners so we got something like
"move to the west" -> ["move", "north"] -> move(west).
also make it work with different names
"go to the cliffs" -> ["go", "cliffs"] -> move(west). write relations that recognize that 'cliffs' and 'west' while in specific states, mean the same thing (same direction)

*/

% analogous to q(ask) in lecture, prompts user to make an input, and tokenizes it, removing punctuation (could change it)
% OutputCommand just reads out what the computer would try to execute
% test in SWI by doing: ?- say(C).
say(OutputCommand) :-
    write("What do you do? "), nl, flush_output(current_output),
    read_line_to_string(user_input, String),
    split_string(String, " -", " ,?.!-", Ln),
    parsecommand(Ln,_,OutputCommand).

% parsecommand(InputCommand, ExecutedOutputCommand) :-
%    command(InputCommand, OutputCommand),
%   execute_command(OutputCommand, ExecutedOutputCommand).

% true if L0 and L2 form difference list that is a legal command
parsecommand(L0,L2, Command) :-
    verb_phrase(L0, L1, C1),
    noun_phrase(L1,L2, C2),
    make_command(C1, C2, Command).

verb_phrase(L0, L2, Ind):-
    verb(L0, L1, Ind),
    prep(L1,L2, Ind).

noun_phrase(L0, L2, Ind) :-
    det(L0, L1, Ind),
    noun(L1, L2, Ind).

make_command(V, N, [V|N]).

det(["the" | L], L,_).
det(["a" | L], L,_).
det(["this" | L], L,_).
det(["that" | L], L,_).
det(L, L,_).

prep(["to"| L], L, _).
prep(["to", "the"| L], L, _).
prep(["into"| L], L, _).
prep(["into", "the"|  L], L, _).
prep(["in", "to"| L], L, _).
prep(["in", "to", "the"| L], L, _).
prep(L, L, _).

verb(["move"| L], L, Ind) :- move_verb(Ind).
verb(["go"| L], L, Ind) :- move_verb(Ind).
verb(["walk"| L], L, Ind) :- move_verb(Ind).

noun(["south" | L], L, Ind) :- south_noun(Ind).
noun(["down" | L], L, Ind) :- south_noun(Ind).

noun(["west" | L], L, Ind) :- west_noun(Ind).
noun(["left" | L], L, Ind) :- west_noun(Ind).

noun(["east" | L], L, Ind) :- east_noun(Ind).
noun(["right" | L], L, Ind) :- east_noun(Ind).

noun(["north" | L], L, Ind) :- north_noun(Ind).
noun(["up" | L], L, Ind) :- north_noun(Ind).

move_verb(move).

north_noun(north).
south_noun(south).
east_noun(east).
west_noun(west).
% north_noun(cliffs) :- % here we check if current state is state where north_state=cliffs for example
