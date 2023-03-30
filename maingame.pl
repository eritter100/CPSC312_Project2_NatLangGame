% maingame
% prolog
%
:- dynamic(current_state/1).
:- dynamic(inventory/1).
:- dynamic(position/1).

% retract current state to start_state for beginning of game
:- retractall(current_state(_)).

% starting inventory (list of strings), (maybe make a triple that corrends name of item to item?)
inventory([]).

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
d :- move(d).
e :- move(e).
f :- move(f).
g :- move(g).

% basic starter to the game
start :-
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



move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, NewState),
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
    position(monster(M), State),
    input(monster(M), Move), 
    retract(position((monster(M)), State)),
    retract(input(monster(M), Move)),
    attack_monster(monster(M)), !.
    
move(_) :-
    write("Invalid move!"), nl, !.





% victory
attack_monster(monster(M)) :-
    get_strength(monster(M), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    write("you won the combat!"), nl,
    describe, nl.
% defeat
attack_monster(monster(M)) :-
    get_strength(monster(M), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    write("you lost the combat!"), nl,
    describe, nl.

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
    write_items([I]),
    write_monsters([I]), fail.

write_state_contents(_).

% writes items to terminal
write_items([]).
write_items([H|T]) :-
    item_name(H,N),
    input(H,I),
    write("Type "), write(I), write(" to take "), write(N), nl,
    write_items(T).
write_items([H|T]) :-
    not(item_name(H,_)),
    write_items(T).

% writes monsters to terminal
write_monsters([]).
write_monsters([H|T]) :-
    monster_name(H,N),
    input(H,I),
    write("Type "), write(I), write(" to attack "), write(N), nl,
    write_monsters(T).
write_monsters([H|T]) :-
    not(monster_name(H,_)),
    write_monsters(T).

exits(State) :-
    path(State, Move, Exit),
    write_exits(Move, Exit), fail.
exits(_).

write_exits(Move, Exit) :-
    write("Type "), write(Move), write(" for "), write(Exit), nl.


:-dynamic(path/3).
% paths describe relation between Current_State, move, next_state / interaction

path(start_state, east, east_state_1).
path(east_state_1, west, start_state).
path(start_state, west, west_state_1).
path(west_state_1, east, start_state).

% path(east_state_1, a, item(sword)).
% path(east_state_1, b, item(shield)).
% path(west_state_1, a, item(potion)).

% path(west_state_1, b, item(zombie)).
% path(west_state_1, c, item(dragon)).
% path(east_state_1, c, item(wizard)).

is_state(start_state).
is_state(east_state_1).
is_state(west_state_1).

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

% MONSTERS
% monsters placed at start of game, later move??????????

:-dynamic(position/2).
:-dynamic(input/2).
position(item(sword), east_state_1).
position(item(shield), east_state_1).
position(item(potion), west_state_1).

position(monster(zombie), west_state_1).
position(monster(dragon), west_state_1).
position(monster(wizard), east_state_1).

input(item(sword), a).
input(item(shield), b).
input(item(potion), a).

input(monster(zombie), b).    % maybe we should have overlapping commands, like "press b to grab that sheild AND attack that zombie - you MUST do both"????
input(monster(dragon), c).
input(monster(wizard), c).

item_name(item(sword), 'Sword').
item_name(item(shield), 'Shield').
item_name(item(potion), 'Potion').

monster_name(monster(zombie), 'Zombie').
monster_name(monster(dragon), 'Dragon').
monster_name(monster(wizard), 'Wizard').

get_player_strength(PlayerStrength) :-
    inventory(Inventory),
    get_sum_inventory_strength(PlayerStrength, Inventory).
get_sum_inventory_strength(0, []).
get_sum_inventory_strength(Strength, [H|T]) :-
    get_strength(item(H), ItemStrength),
    get_sum_inventory_strength(ListStrength, T),
    Strength == ListStrength + ItemStrength.
get_strength(item(sword), 3).
get_strength(item(shield), 3).
get_strength(item(potion), 3).
get_strength(monster(zombie), 4).
get_strength(monster(dragon), 4).
get_strength(monster(wizard), 4).