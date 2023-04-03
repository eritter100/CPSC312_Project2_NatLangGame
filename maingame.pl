% maingame
% prolog
%
:- dynamic(current_state/1).
:- dynamic(inventory/1).
:- dynamic(position/1).
:- dynamic(life_status/1).
:- dynamic(tutorial_needed/1).

% retract current state to start_state for beginning of game
:- retractall(current_state(_)).
:- retractall(inventory(_)).

% starting inventory (list of strings), (maybe make a triple that corrends name of item to item?)
inventory([]).
current_state(start_state).
life_status(alive).
tutorial_needed(yes).

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
    current_state(State),
    inventory(Inventory),
    reset_state_items(Inventory),
    retract(inventory(Inventory)),
    assert(inventory([])), % add reset states func 
    retract(current_state(State)),
    assert(current_state(start_state)),
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(alive)),
    describe, !.

% describe is used to describe the current state 
describe :-
    life_status(alive),
    current_state(State),
    write_state(State).

describe :-
    write("You are dead! There is nothing to describe!").
% write_state writes out the environment of every state to the screen, as well as how they connect to other states
write_state(start_state) :-
    tutorial_needed(Status), (\+ dif(yes, Status)),
    tutorial, nl, nl, nl,
    retract(tutorial_needed(Status)),
    assert(tutorial_needed(no)),
    write("To the west you see zaps of bright lights breaking from atop the cliffside."), nl,
    write("To the east, a worn down gravel path, darkened by the canopy of the forest."), nl,
    write("To the north, a massive limestone wall with a large black gate."), nl, nl,
    write_state_contents(start_state), nl,
    exits(start_state), nl.

write_state(start_state) :-
    write("You stand in the center of a lavender heath surrounded by cliffs and forest."), nl,
    write("To the west you see zaps of bright lights breaking from atop the cliffside."), nl,
    write("To the east, a worn down gravel path, darkened by the canopy of the forest."), nl, nl,
    write_state_contents(start_state), nl,
    exits(start_state), nl.

write_state(west_state_1) :-
    write("You are on atop the cliffside. The land is barren and wind is fierce."), nl, nl,
    write("To the east is the peaceful, lavender heath."), nl,
    write("Beside you on a granite rock, a large key someone must have forgotten."), nl,
    write("At the opposite end of the cliffside a wizard is practicing his spells, shooting violent black and purple zaps of lightening."), nl, nl,
    write_state_contents(west_state_1), nl,
    exits(west_state_1), nl.

write_state(east_state_1) :-
    write("You are in the middle of the forest. It's damp, dark, you can barely see. Your feet sink into the mossy dirt beneath you."), nl, nl,
    write("To the west is the peaceful lavender heath."), nl,
    write("To your right, a tiny, rusted sword. You don't think about the one who dropped it."), nl,
    write("To your left, a heavy shield. You really don't want to think about who dropped it."), nl,
    write("Just past a large, rotting stump you see a zombie hunched over something."), nl, nl,
    write_state_contents(east_state_1), nl,
    exits(east_state_1), nl.

write_state(north_state_1) :-
    write("You are at the foot of the black gate. The limestone wall goes on for what seems like forever. The ground rumbles below your feet."), nl, nl,
    write("To the south is the peaceful lavender heath."), nl,
    write("Behind the gate you see plumes of dark smoke rising into the air."), nl, nl,
    write_state_contents(north_state_1), nl,
    exits(north_state_1), nl.

write_state(north_state_2) :-
    write("You are in some sort of dragon nest. Carcasses and old scales scatter the ground."), nl, nl,
    write("To the south is the open gate."), nl, 
    write("Infront of you is a massive, red dragon. He's busy with his lunch so he doesn't notice you at first."), nl, nl,
    write_state_contents(north_state_2), nl,
    exits(north_state_2), nl.


move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, NewState, LockStatus),
    unlock(PreviousState, Move, NewState, LockStatus),
    is_state(NewState), 
    retract(current_state(PreviousState)),
    assert(current_state(NewState)),
    life_status(alive),
    describe, !.

move(Move) :-
    current_state(State),
    position(item(I), State),
    input(item(I), Move), 
    retract(position((item(I)), State)),
    retract(input(item(I), Move)),
    life_status(alive),
    add_to_inventory(item(I)), !.

move(Move) :-
    current_state(State),
    position(person(P), State),
    input(person(P), Move),
    life_status(alive),
    interaction(person(P)), !.
move(_) :-
    life_status(alive),
    write("Invalid move!"), nl, !.

move(_) :-
    life_status(dead),
    write("Ummm... your dead!"), nl, !.

% DRAGON ENCOUNTER
% 3 interactions with the dragon, 1 win, 1 loss with shield, 1 loss completely
interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(magic_sword), CurrentInventory),
    member(item(shield), CurrentInventory),
    write("You repel the dragon's flames with your shield, and pierce the thick scales on his neck with your magic sword!"), nl,
    write("You have beat the adventure game!"), nl,
    write("Type halt. to quit out entirely, or start. to do it again!").

interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But with no weapon, you cannot damage the dragon and grow too tired to fight!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely, or start. to try again!").

interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    member(item(sword), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But your simple sword can't pierce his tough skin! The dragon won't take any damage!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely, or start. to try again!").

interaction(person(dragon)) :-
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(dead)),
    write("The dragon burns you with his fiery breath!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely, or start. to try again!").

% ZOMBIE ENCOUNTER
% victory - if player is strong enough to defeat zombie (based on contents of inventory),
% then an item in inventory is sacrificed to defeat zombie
interaction(person(zombie)) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength > MonsterStrength,
    inventory([I1|_]),
    item_name(I1, ItemName),
    write("You desperately throw your "), write(ItemName), write(" at the zombie."), nl,
    remove_from_inventory(I1),
    write("Narrowly, you snatch victory from the jaws of defeat! The zombie is vanquished!"), nl,
    write("You won!"), nl,
    describe, nl.
% defeat - if player is NOT strong enough to defeat zombie (based on contents of inventory)
interaction(person(zombie)) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(dead)),
    write("Oh no! The zombie ate your brain!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely, or start. to try again!").

% WIZARD ENCOUNTER
% friendly encounter - wizard upgrades the players sword to a magic sword
interaction(person(wizard)) :-
    inventory(Inventory),
    member(item(sword), Inventory),
    remove_from_inventory(item(sword)),
    add_to_inventory(item(magic_sword)),
    write("The wizard has upgraded your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    describe, nl.
% unfriendly encounter - wizard downgrades the players magic sword to a sword
interaction(person(wizard)) :-
    inventory(Inventory),
    member(item(magic_sword), Inventory),
    remove_from_inventory(item(magic_sword)),
    add_to_inventory(item(sword)),
    write("Oh no! The wizard has removed the magic from your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    describe, nl.
% unfriendly encounter - wizard attacks if the player doesnt have any sword
interaction(person(wizard)) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength > MonsterStrength,
    add_to_inventory(item(gold)),
    write("You mugged the wizard and took his gold!"), nl,
    describe, nl.
interaction(person(wizard)) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    add_to_inventory(item(gold)),
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(dead)),
    write("Oh no! The wizard turned you into a frog!"), nl,
    write("You lose!"), nl,
    write("Type halt. to quit out entirely, or start. to try again!").

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
    nl.

remove_from_inventory(item(I)) :-
    inventory(OldInventory),
    item_name(item(I), Name),
    list_remove(item(I), OldInventory, UpdatedInventory),
    retract(inventory(OldInventory)),
    assert(inventory(UpdatedInventory)),
    write(Name), write(" has been removed from your inventory."), nl,
    nl.

list_add(I, [], [I]).
list_add(I, [H|T], [I|[H|T]]).

% Resets items in states according to inventory
% gold doesnt effect states
reset_state_items([item(gold)|T]) :-
    reset_state_items(T).
% reset sword to east_state_1, including if upgraded to magic sword
reset_state_items([H|T]) :-
    \+ dif(H, item(sword)),
    \+ (position(H, east_state_1)),
    assert(position(H, east_state_1)),
    assert(input(H, a)),
    reset_state_items(T).
reset_state_items([H|T]) :-
    \+ dif(H, item(magic_sword)),
    \+ (position(item(sword), east_state_1)),
    assert(position(item(sword), east_state_1)),
    assert(input(item(sword), a)),
    reset_state_items(T).
% reset shield to east_state_1
reset_state_items([H|T]) :-
    \+ dif(H, item(shield)),
    \+ (position(H, east_state_1)),
    assert(position(H, east_state_1)),
    assert(input(H, b)),
    reset_state_items(T).
% reset key to west_state_1
reset_state_items([H|T]) :-
    \+ dif(H, item(key)),
    \+ (position(H, west_state_1)),
    assert(position(H, west_state_1)),
    assert(input(H, a)),
    reset_state_items(T).
% if key was used (not in inventory when restarting) then reset key
reset_state_items([]) :-
    \+ (position(item(key), west_state_1)),
    assert(position(item(key), west_state_1)),
    assert(input(item(key), a)).
% base case
reset_state_items([]).

% call bag to check contents of inventory
bag :-
    life_status(alive),
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
    state_name(Exit, Name),
    write("Type "), write(Move), write(" to go to "), write(Name), nl.

write_exits(Move, Exit, locked) :-
    state_name(Exit, Name),
    write("Type "), write(Move), write(" to  "), write(Name), write(" [LOCKED]"), nl.

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

% path(west_state_1, b, item(zombie)).
% path(west_state_1, c, item(dragon)).
% path(east_state_1, c, item(wizard)).

is_state(start_state).
is_state(east_state_1).
is_state(west_state_1).
is_state(north_state_1).
is_state(north_state_2).

tutorial :-
    write("Welcome to the game, this is a text-based adventure game!"), nl,
    write("Current basic rules are as follows:"), nl,
    write("To move in a direction, type one of: east, west, south, north, followed by a ."), nl,
    write("Type 'help.' for more help on commands"), nl, nl, nl, nl,
    write("You wake up in the middle of a peaceful, lavender heath surrounded by forest and cliffs. Some may even call it the central heath."), nl.
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
position(item(key), west_state_1).
position(person(dragon), north_state_2).
position(person(zombie), east_state_1).
position(person(wizard), west_state_1).
input(item(sword), a).
input(item(shield), b).
input(item(key), a).
input(person(dragon), a).   % should we make these overlap? like "press b to grab that sheild AND attack that zombie - you MUST do neither or both"????
input(person(zombie), c).
input(person(wizard), b).
item_name(item(sword), 'Sword').
item_name(item(shield), 'Shield').
item_name(item(key), 'Key').
item_name(item(magic_sword), 'Magic Sword').
item_name(item(gold), 'Gold Bullion').
person_name(person(dragon), "Boss Dragon").
person_name(person(zombie), "zombie").
person_name(person(wizard), "wizard").
state_name(west_state_1, "cliffs").
state_name(start_state, "lavender heath").
state_name(east_state_1, "dark forest path").
state_name(north_state_1, "black gate").
state_name(north_state_2, "open gate").

get_player_strength(PlayerStrength) :-
    inventory(Inventory),
    get_sum_inventory_strength(PlayerStrength, Inventory).
get_sum_inventory_strength(0, []).
get_sum_inventory_strength(Strength, [H|T]) :-
    get_strength(H, ItemStrength),
    get_sum_inventory_strength(ListStrength, T),
    Strength is ListStrength + ItemStrength.
get_strength(item(sword), 3).
get_strength(item(shield), 2).
get_strength(item(key), 0).
get_strength(item(magic_sword), 6).
get_strength(item(gold), 1).
get_strength(person(zombie), 4).
get_strength(person(dragon), 4).
get_strength(person(wizard), 4).

