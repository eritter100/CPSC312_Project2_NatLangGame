% maingame
% prolog
%
:- dynamic(current_state/1).
:- dynamic(inventory/1).
:- dynamic(removed_person_list/1).
:- dynamic(position/1).
:- dynamic(life_status/1).
:- dynamic(tutorial_needed/1).

% retract current state to start_state for beginning of game
:- retractall(current_state(_)).
:- retractall(inventory(_)).
:- retractall(removed_person_list(_)).

% starting inventory (list of strings), (maybe make a triple that corrends name of item to item?)
inventory([]).
removed_person_list([]).
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
    remove_inputs_from_location(State),
    removed_person_list(Characters),
    return_characters_from_removed_person_list(Characters),
    retract(removed_person_list(Characters)),
    assert(removed_person_list([])),
    inventory(Inventory),
    reset_state_items(Inventory),
    retract(inventory(Inventory)),
    assert(inventory([])), % add reset states func 
    retract(current_state(State)),
    assert(current_state(start_state)),
    add_inputs_to_location(State),
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
    describe_neighbours, nl,
    write_state_contents(start_state), nl,
    exits(start_state), nl.

write_state(State) :-
    describe_current_location, nl,
    describe_neighbours, nl,
    long_describe_contents(State), nl,
    write_state_contents(State), nl,
    exits(State), nl.


move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, NewState, LockStatus),
    unlock(PreviousState, Move, NewState, LockStatus),
    is_state(NewState),
    remove_inputs_from_location(PreviousState),
    add_inputs_to_location(NewState),
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
    add_to_inventory(item(I)), describe, !.

move(Move) :-
    current_state(State),
    position(person(P), State),
    input(person(P), Move),
    life_status(alive),
    interaction(person(P)),
    add_to_removed_person_list(position(person(P), State)),
    retract(position(person(P), State)),
    retract(input(person(P), Move)), describe,
    !.
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
    restart_instructions.

interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But with no weapon, you cannot damage the dragon and grow too tired to fight!"), nl,
    die.

interaction(person(dragon)) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    member(item(sword), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But your simple sword can't pierce his tough skin! The dragon won't take any damage!"), nl,
    die.

interaction(person(dragon)) :-
    write("The dragon burns you with his fiery breath!"), nl,
    die.

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
    nl.
% defeat - if player is NOT strong enough to defeat zombie (based on contents of inventory)
interaction(person(zombie)) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    write("Oh no! The zombie ate your brain!"), nl,
    die.

% WIZARD ENCOUNTER
% friendly encounter - wizard upgrades the players sword to a magic sword
interaction(person(wizard)) :-
    inventory(Inventory),
    member(item(sword), Inventory),
    remove_from_inventory(item(sword)),
    add_to_inventory(item(magic_sword)),
    write("The wizard has upgraded your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    nl.
% unfriendly encounter - wizard downgrades the players magic sword to a sword
interaction(person(wizard)) :-
    inventory(Inventory),
    member(item(magic_sword), Inventory),
    remove_from_inventory(item(magic_sword)),
    add_to_inventory(item(sword)),
    write("Oh no! The wizard has removed the magic from your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    nl.
% unfriendly encounter - wizard attacks if the player doesnt have any sword
interaction(person(wizard)) :-
    get_strength(person(wizard), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength > MonsterStrength,
    add_to_inventory(item(gold)),
    write("You mugged the wizard and took his gold!"), nl,
    nl.
interaction(person(wizard)) :-
    get_strength(person(wizard), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    add_to_inventory(item(gold)),
    write("Oh no! The wizard turned you into a frog!"), nl,
    die.

% WISHING WELL ENCOUNTER
% friendly encounter - well upgrades the players gold to a magic sword
interaction(person(well)) :-
    inventory(Inventory),
    member(item(gold), Inventory),
    remove_from_inventory(item(gold)),
    add_to_inventory(item(magic_sword)),
    write("On impulse, you toss your sack of gold bullion into the well."), nl,
    write("The well seems to glow with rainbows. Suddenly you find yourself holding a magic sword!"), nl,
    write("As the multi-coloured light fades, you notice that the well has vanished."), nl,
    nl.
% friendly encounter - is you have a normal sword, the well summons a wizard
interaction(person(well)) :-
    inventory(Inventory),
    member(item(sword), Inventory),
    current_state(State),
    assert(position(person(wizard), State)),
    get_next_option_index(State, Option),
    assert(input(person(wizard), Option)),
    write("The wishing well growls ominously."), nl,
    write("You draw your sword to attack, but suddenly the well transforms into a wizard!"), nl,
    nl.
% neutral encounter - the well gives you life advice and teleports you somewhere else
interaction(person(well)) :-
    write("You lean over the well, entranced."), nl,
    write("\'Oh brave hero!\' The well calls out, \'You must defeat the dragon! To do that, you must have a magic sword."),
    write(" To reach the dragon you must also have a key.\'"), nl,
    write("Panicking, you walk into the well and... land in some strange new place"), nl,
    current_state(State),
    retract(current_state(State)),
    assert(current_state(start_state)),
    move(north),
    nl.

map :-
    inventory(I),
    member(item(gameMap), I),
    current_state(State),
    state_name(State, Name),
    write("You are currently at the "), write(Name), nl,
    write_neighbours(State), !.
map :-
    inventory(I),
    not(member(item(gameMap), I)),
    write("Sorry! You don't have a map!"), nl.
write_neighbours(State) :-
    write_neighbour(north, State, "   "),
    write_neighbour(east, State, "   "),
    write_neighbour(south, State, "   "),
    write_neighbour(west, State, "   ").
write_neighbour(Direction, State, Tabs) :-
    path(State, Direction, NewLocation, _),
    write(Tabs), state_name(NewLocation, Name),
    write(Tabs), write("To the "), write(Direction), write(" there is a "), write(Name), nl,
    string_concat("   ", Tabs, NewTabs),
    adjascent_cardinal_positions(Direction, RightDirection),
    adjascent_cardinal_positions(LeftDirection, Direction),
    write_neighbour(Direction, NewLocation, NewTabs),
    write_neighbour(RightDirection, NewLocation, NewTabs),
    write_neighbour(LeftDirection, NewLocation, NewTabs).
write_neighbour(_, _, _).

adjascent_cardinal_positions(north, east).
adjascent_cardinal_positions(east, south).
adjascent_cardinal_positions(south, west).
adjascent_cardinal_positions(west, north).
    
    
% assertions and prints after player death
die :-
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(dead)),
    write("You lose!"), nl,
    restart_instructions.

% instruct player on how to restart game (upon clear or death)
restart_instructions :-
    write("Type halt. to quit out entirely, or start. to try again!"), nl.

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

add_to_removed_person_list(position(person(P), State)) :-
    removed_person_list(Old_person_list),
    list_add(position(person(P), State), Old_person_list, New_person_list),
    retract(removed_person_list(Old_person_list)),
    assert(removed_person_list(New_person_list)).
    
% return wizard / zombie / dragon / etc. to start position at start of each game
return_characters_from_removed_person_list([position(person(P), State)|T]) :-
    assert(position(person(P), State)),
    get_next_option_index(State, Option),
    assert(input(person(P), Option)),
    return_characters_from_removed_person_list(T).
return_characters_from_removed_person_list([]).

% next option
next_option(a,b).
next_option(b,c).
next_option(c,d).
next_option(d,e).

get_next_option_index(State, Option) :-
    position(I, State),
    increment_contents([I], a, Option). % , fail.
increment_contents([_|T], CurrentOption, FinalOption) :-
    next_option(CurrentOption, NextOption),
    increment_contents(T, NextOption, FinalOption).
increment_contents([], CurrentOption, FinalOption) :-
    next_option(CurrentOption, FinalOption).

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


tutorial :-
    write("Welcome to the game, this is a text-based adventure game!"), nl,
    write("Current basic rules are as follows:"), nl,
    write("To move in a direction, type one of: east, west, south, north, followed by a ."), nl,
    write("Type 'help.' for more help on commands"), nl,
    write("Type 'map.' to see a map of the game"), nl, nl, nl, nl,
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
position(item(gameMap), south_east_state_1).
position(person(well), south_state_1).
position(person(dragon), north_state_2).
position(person(zombie), east_state_1).
position(person(wizard), west_state_1).
position(person(wizard), south_east_state_1).
% input(item(sword), a).
% input(item(shield), b).
% input(item(key), a).
% input(person(dragon), a).
% input(person(zombie), c).
% input(person(wizard), b).
% input(person(well), a).
item_name(item(sword), 'Sword').
item_name(item(shield), 'Shield').
item_name(item(key), 'Key').
item_name(item(magic_sword), 'Magic Sword').
item_name(item(gold), 'Gold Bullion').
item_name(item(gameMap), 'Map').
person_name(person(dragon), "Boss Dragon").
person_name(person(zombie), "zombie").
person_name(person(wizard), "wizard").
person_name(person(well), 'Magic Well').

description_long(item(sword), "a tiny, rusted sword. You don't think about the one who dropped it").
description_long(item(shield), "a heavy shield, cracked and bloodstained. You REALLY don't want to think about who dropped it.").
description_long(item(key), "a large key someone must have forgotten.").
description_long(item(magic_sword), "a greatsword, massive and glowing with epic glory.").
description_long(item(gold), "a large, stained sack of gold.").
description_long(item(gameMap), "a large, crumbling parchment map.").
description_long(person(dragon), "a massive, red dragon. He's busy with his lunch so he doesn't notice you at first.").
description_long(person(zombie), "a zombie drooling brains.").
description_long(person(wizard), "a wizard is practicing his spells, shooting violent black and purple zaps of lightening.").
description_long(person(well), "a magic well that seems to be whispering your destiny to you.").

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
get_strength(item(gameMap), 0).
get_strength(person(zombie), 4).
get_strength(person(dragon), 4).
get_strength(person(wizard), 4).
get_strength(person(well), 0).

% LOCATION STUFF

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
path(east_state_1, south, south_east_state_1, unlocked).
path(south_east_state_1, north, east_state_1, unlocked).
path(south_state_1, east, south_east_state_1, unlocked).
path(south_east_state_1, west, south_state_1, unlocked).

:- dynamic(current_input_val/1).
:- retractall(current_input_val(_)).
current_input_val(a).

add_inputs_to_location(Location) :-
    position(PersonItem, Location),
    current_input_val(CurrentInput),
    not(input(PersonItem, _)),
    assert(input(PersonItem, CurrentInput)),
    next_option(CurrentInput, NextInput),
    retract(current_input_val(CurrentInput)),
    assert(current_input_val(NextInput)),
    add_inputs_to_location(Location).
add_inputs_to_location(_).

remove_inputs_from_location(Location) :-
    position(PersonItem, Location),
    input(PersonItem, Input1),
    retract(input(PersonItem, Input1)),
    current_input_val(Input2),
    retract(current_input_val(Input2)),
    assert(current_input_val(a)),
    remove_inputs_from_location(Location).
remove_inputs_from_location(_).

is_state(start_state).
is_state(east_state_1).
is_state(west_state_1).
is_state(north_state_1).
is_state(north_state_2).
is_state(south_east_state_1).
is_state(south_state_1).

state_name(west_state_1, "cliffs").
state_name(start_state, "lavender heath").
state_name(east_state_1, "dark forest path").
state_name(north_state_1, "black gate").
state_name(north_state_2, "open gate").
state_name(south_east_state_1, "ominous jungle").
state_name(south_state_1, "stormy seaside").

% describing the location from the next location over
state_neighbour_description(west_state_1, "you see zaps of bright lights breaking from atop the cliffside.").
state_neighbour_description(start_state, "the peaceful, lavender heath.").
state_neighbour_description(east_state_1, "a worn down gravel path, darkened by the canopy of the forest.").
state_neighbour_description(north_state_1, "a massive limestone wall with a large black gate.").
state_neighbour_description(north_state_2, "behind the gate you see plumes of dark smoke rising into the air.").
state_neighbour_description(south_east_state_1, "through the trees you hear the terrible screams of jaguars and flesh-eating parrots.").
state_neighbour_description(south_state_1, "beyond, you see a stormy beach, littered in jagged rocks.").

% describe the current location
state_current_description(west_state_1, "You are on atop the cliffside. The land is barren and wind is fierce.").
state_current_description(start_state, "You stand in the center of a lavender heath surrounded by cliffs and forest.").
state_current_description(east_state_1, "You are in the middle of the forest. It's damp, dark, you can barely see. Your feet sink into the mossy dirt beneath you.").
state_current_description(north_state_1, "You are at the foot of the black gate. The limestone wall goes on for what seems like forever. The ground rumbles below your feet.").
state_current_description(north_state_2, "You are in some sort of dragon nest. Carcasses and old scales scatter the ground.").
state_current_description(south_east_state_1, "You are in a jungle. Parrots with sharp beaks stare down at you, and all around brightly coloured carnivorous flowers seem to smile at you.").
state_current_description(south_state_1, "You are on a windswept beach. The waves crash on the shoreline with a deafening roar, soaking you in freezing seaspray.").

% describing directions to locations
direction_description(east, "To the east, ").
direction_description(north, "To the north, ").
direction_description(west, "To the west, ").
direction_description(south, "To the south, ").

% describe placement of items, thematically related to location
item_prefix(west_state_1, a, "Beside you on a granite rock, ").
item_prefix(west_state_1, b, "At the opposite end of the cliffside ").
item_prefix(west_state_1, c, "In a small damp cave, you see ").
item_prefix(west_state_1, d, "Hanging off a small windswept tree with scars from lightning strikes ").
item_prefix(west_state_1, e, "Half-buried under a small rockslide is ").
item_prefix(start_state, a, "Half-hidden under the flowers, you notice ").
item_prefix(start_state, b, "At your feet there is ").
item_prefix(start_state, c, "Amid a swarm of beautiful butterflies, ").
item_prefix(start_state, d, "Beside you, ").
item_prefix(start_state, e, "Sitting in the sunlight, ").
item_prefix(east_state_1, a, "On the mossy earth, ").
item_prefix(east_state_1, b, "Leaning against a large gnarled tree, ").
item_prefix(east_state_1, c, "Just past a large, rotting stump you see ").
item_prefix(east_state_1, d, "Hanging off a nearby tree branch, there is ").
item_prefix(east_state_1, e, "Sitting in a muddy creek, ").
item_prefix(north_state_1, a, "To your right, ").
item_prefix(north_state_1, b, "To your left, ").
item_prefix(north_state_1, c, "Sitting before you, there is ").
item_prefix(north_state_1, d, "In a small crevase tucked behind some menacing stalactites, you notice ").
item_prefix(north_state_1, e, "At your feet, you see ").
item_prefix(north_state_2, a, "Half-buried in charred bones, there is ").
item_prefix(north_state_2, b, "Beneath a heap of leathern scales, you see ").
item_prefix(north_state_2, c, "Behind a smoking mound, ").
item_prefix(north_state_2, d, "Sitting in a clutch of dragon eggs, there is ").
item_prefix(north_state_2, e, "Beside you, ").
item_prefix(south_east_state_1, a, "Leaning against a tree, there is ").
item_prefix(south_east_state_1, b, "Beyond a pirana-infested stream, you see ").
item_prefix(south_east_state_1, c, "Lurking in the shadows, you notice ").
item_prefix(south_east_state_1, d, "To your right, ").
item_prefix(south_east_state_1, e, "To your left, ").
item_prefix(south_state_1, a, "Half-buried in sand, there is ").
item_prefix(south_state_1, b, "Sitting in the shallows, you see ").
item_prefix(south_state_1, c, "Bobbing in the waves, you notice ").
item_prefix(south_state_1, d, "Between some rocky spires, ").
item_prefix(south_state_1, e, "At the high-tide line, ").

long_describe_contents(State) :-
%    current_state(State),
    position(I, State),
    long_describe_item([I], State), fail.
long_describe_contents(_).

long_describe_item([], _).
long_describe_item([H|T], State) :-
    input(H, InputIndex),
    item_prefix(State, InputIndex, DescriptionPart1),
    description_long(H, DescriptionPart2),
    write(DescriptionPart1), write(DescriptionPart2), nl,
    long_describe_item(T, State).

describe_current_location :-
    current_state(State),
    state_current_description(State, Description),
    write(Description), nl.

describe_neighbours :-
    describe_neighbour(north),
    describe_neighbour(south),
    describe_neighbour(east),
    describe_neighbour(west).

describe_neighbour(Direction) :-
    current_state(State),
    path(State, Direction, NeighbourState, _),
    direction_description(Direction, DescriptionPart1),
    state_neighbour_description(NeighbourState, DescriptionPart2),
    write(DescriptionPart1), write(DescriptionPart2), nl.
% if there isn't a location in that direction, don't write anything
describe_neighbour(Direction) :-
    current_state(State),
    not(path(State, Direction, _, _)).