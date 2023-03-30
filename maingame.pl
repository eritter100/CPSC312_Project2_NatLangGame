% maingame
% prolog
%
:- dynamic(current_state/1).
:- dynamic(inventory/1).
:- dynamic(position/1).

% retract current state to start_state for beginning of game
:- retractall(current_state(_)).
:- retractall(inventory(_)).

% starting inventory (list of strings), (maybe make a triple that corrends name of item to item?)
inventory([]).
current_state(start_state).

% basic movement inputs , not nat-lang yet
% for direction
east :- move(east).
west :- move(west).
north :- move(north).
south :- move(south).

% basic controls for interactions
a :- move(a).
b :- move(b).
c :- move(c).

% basic starter to the game
start :-
    current_state(State),
    inventory(Inventory),
    retract(inventory(Inventory)),
    assert(inventory([])), % add reset states func
    retract(current_state(State)),
    assert(current_state(start_state)),
    describe, !.

% describe is used to describe the current state 
describe :-
    current_state(State),
    write_state(State).

% write_state writes out the environment of every state to the screen, as well as how they connect to other states
write_state(start_state) :-
    tutorial, nl, nl, nl,
    write("You are in start_state right now"), nl,
    write("Contents of start_state are as follows:"), nl,
    write_state_contents(start_state), nl,
    exits(start_state), nl.

write_state(east_state_1) :-
    write("You are in east_state_1 right now"), nl, nl,
    write("Contents of east_state_1 are as follows:"), nl, nl,
    write_state_contents(east_state_1), nl,
    exits(east_state_1), nl.

write_state(west_state_1) :-
    write("You are in west_state_1 right now"), nl, nl,
    write("Contents of west_state_1 are as follows:"), nl, nl,
    write_state_contents(west_state_1), nl,
    exits(west_state_1), nl.

write_state(north_state_1) :-
    write("You are in north_state_1 right now. To north is a large, black gate shrouded in plumes of smoke."), nl, nl,
    write("Contents of north_state_1 are as follows:"), nl, nl,
    write_state_contents(north_state_1), nl,
    exits(north_state_1), nl.

write_state(north_state_2) :-
    write("You are in north_state_2 right now"), nl, nl,
    write("Contents of north_state_2 are as follows:"), nl, nl,
    write_state_contents(north_state_2), nl,
    exits(north_state_2), nl.


move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, NewState, LockStatus),
    unlock(PreviousState, Move, NewState, LockStatus),
    is_state(NewState), 
    retract(current_state(PreviousState)),
    assert(current_state(NewState)),
    describe, !.

move(Move) :-
    current_state(State),
    position(item(I), State),
    input(item(I), Move), 
    retract(position((item(I)), State)),
    retract(input(item(I), Move)),
    add_to_inventory(item(I)), !.

move(Move) :-
    current_state(State),
    position(person(P), State),
    input(person(P), Move),
    interaction(person(P)), !.
move(_) :-
    write("Invalid move!"), nl, !.

% 3 interactions with the dragon, 1 win, 1 loss with shield, 1 loss completely
interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(sword), CurrentInventory),
    member(item(shield), CurrentInventory),
    write("You repel the dragon's flames with your shield, and slice his neck with your sword!"), nl,
    write("You have beat the adventure game!"), nl,
    write("Type halt. to quit out entirely").

interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But with no weapon, you cannot damage the dragon and grow too tired to fight!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely").

interaction(person(dragon)) :-
    write("The dragon burns you with his fiery breath!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely").

unlock(_,_,_,unlocked). % given any unlocked state, state is unlock
unlock(PathState, Move, State, locked) :-
    inventory(CurrentInventory),
    member(item(key), CurrentInventory),
    write("Used key! Door has been unlocked!"), nl,
    list_remove(item(key), CurrentInventory, NewInventory),
    retract(inventory(CurrentInventory)),
    assert(inventory(NewInventory)),
    retract(path(PathState, Move, State, locked)),
    assert(path(PathState, Move, State, unlocked)).

% removes only first instance of element
list_remove(_, [], []).
list_remove(X, [X|T], T).
list_remove(X, [H|T], [H|L]) :-
    dif(X,H),
    list_remove(X,T,L).



add_to_inventory(item(I)) :-
    inventory(OldInventory),
    item_name(item(I), Name),
    list_add(item(I), OldInventory, UpdatedInventory),
    retract(inventory(OldInventory)),
    assert(inventory(UpdatedInventory)),
    write(Name), write(" has been added to your inventory!"), nl,
    describe, nl.

list_add(I, [], [I]).
list_add(I, [H|T], [I|[H|T]]).



% call bag to check contents of inventory
bag :-
    inventory(Inventory),
    write("Here is the contents of your bag:"), nl,
    write_bag(Inventory), !.

% writes contents of inventory to terminal
write_bag([]) :- write("").
write_bag([item(H)|T]) :-
    item_name(item(H), Name),
    write(Name), nl,
    write_bag(T).

% gets all contents in state and writes them to terminal
write_state_contents(State) :-
    position(I, State),
    write_contents([I]), fail.

write_state_contents(_).

% writes items to terminal
write_contents([]).
write_contents([H|T]) :-
    item_name(H,N),
    input(H,I),
    write("Type "), write(I), write(" for "), write(N), nl,
    write_contents(T).
write_contents([H|T]) :-
    person_name(H,N),
    input(H,I),
    write("Type "), write(I), write(" to interact with "), write(N), nl,
    write_contents(T).

exits(State) :-
    path(State, Move, Exit, LockStatus),
    write_exits(Move, Exit, LockStatus), fail.
exits(_).

write_exits(Move, Exit, unlocked) :-
    write("Type "), write(Move), write(" for "), write(Exit), nl.

write_exits(Move, Exit, locked) :-
    write("Type "), write(Move), write(" for "), write(Exit), write(" [LOCKED]"), nl.

:-dynamic(path/4).
% paths describe relation between Current_State, move, next_state, and next_state_lock_status

path(start_state, east, east_state_1, unlocked).
path(east_state_1, west, start_state, unlocked).
path(start_state, west, west_state_1, unlocked).
path(west_state_1, east, start_state, unlocked).
path(start_state, north, north_state_1, unlocked).
path(north_state_1, north, north_state_2, locked). % game finale state
path(north_state_2, south, north_state_1, unlocked).
path(north_state_1, south, start_state, unlocked).

% path(east_state_1, a, item(sword)).
% path(east_state_1, b, item(shield)).
% path(west_state_1, a, item(potion)).

is_state(start_state).
is_state(east_state_1).
is_state(west_state_1).
is_state(north_state_1).
is_state(north_state_2).

tutorial :-
    write("Welcome to the game, this is a text-based adventure game!"), nl,
    write("Current basic rules are as follows:"), nl,
    write("To move in a direction, type one of: east, west, south, north, followed by a ."), nl,
    write("Type 'help.' for more help on commands"), nl.
help :-
    write("list of all commands go here").

% ITEMS
% Items their position in start of game. Currently only in states, future could be in chests, boxes, given to player by NPC etc.
% may change formatting so that its: item(name, property, value), (ex, item(sword, position, east_state_1)) 

:-dynamic(position/2).
:-dynamic(input/2).
position(item(sword), east_state_1).
position(item(shield), east_state_1).
position(item(key), west_state_1).
position(person(dragon), north_state_2).
input(item(sword), a).
input(item(shield), b).
input(item(key), a).
input(person(dragon), a).
item_name(item(sword), 'Sword').
item_name(item(shield), 'Shield').
item_name(item(key), 'Key').
person_name(person(dragon), "Boss Dragon").