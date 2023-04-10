% gamefile with natural language processing

:- dynamic(current_state/1).
:- dynamic(inventory/1).
:- dynamic(removed_person_list/1).
:- dynamic(position/1).
:- dynamic(life_status/1).
:- dynamic(tutorial_needed/1).
:- dynamic(game_won/1).

% retract current state to start_state for beginning of game
% :- retractall(current_state(_)).
% :- retractall(inventory(_)).
% :- retractall(removed_person_list(_)).

% starting inventory (list of strings), (maybe make a triple that corrends name of item to item?)
inventory([]).
removed_person_list([]).
current_state(start_state).
life_status(alive).
tutorial_needed(yes).
game_won(no).

% basic movement inputs , not nat-lang yet
% for direction
/*
east :- move(east).
west :- move(west).
north :- move(north).
south :- move(south).
*/
% basic controls for interactions
/*
a :- move(a).
b :- move(b).
c :- move(c).
d :- move(d).
e :- move(e).
f :- move(f).
g :- move(g).
*/
% basic starter to the game
start :-
    shuffle_map,
    current_state(State),
    inventory(Inventory),
    reset_state_items(Inventory),
    retract(inventory(Inventory)),
    assert(inventory([])), % add reset states func 
    removed_person_list(Characters),
    return_characters_from_removed_person_list(Characters),
    retract(removed_person_list(Characters)),
    assert(removed_person_list([])),
    retract(current_state(State)),
    assert(current_state(start_state)),
    life_status(Status),
    retract(life_status(Status)),
    assert(life_status(alive)),
    game_won(GameStatus),
    retract(game_won(GameStatus)),
    assert(game_won(no)),
    describe, gameloop.

% loops game for every input you give, only stopping when game_over is true (win, die, quit, reset)
gameloop :-
    repeat,
    say(InputCommand, "What do you do? "),
    parsecommand(InputCommand,_,OutputCommand),
    execute_command(OutputCommand), 
    game_over_command(OutputCommand), !.

% NATURAL LANGUAGE PROCESSOR
% prompts user to input, and tokenizes input into list
say(Ln, Prompt) :-
    write(Prompt), nl, flush_output(current_output),
    read_line_to_string(user_input, String),
    split_string(String, " -", " ,?.!-", Ln), !.

% true if L0 and L2 form difference list that is a legal command
parsecommand(L0,L2, Command) :-
    verb_phrase(L0, L1, C1),
    noun_phrase(L1,L2, C2),
    make_command_list(C1, C2, Command), !.
% for simple commands that are verb only
parsecommand(L0,L1, Command) :-
    verb_phrase(L0, L1, C1),
    make_command_list(C1, [], Command), !.
% for commands that are unparseable (nonsense, no context, etc.)
parsecommand(_,_,_) :-
    write("What are you thinking!"), nl, fail.

% L0 and L2 make up a difference list that is a legal verb phrase (verb+preposition (to))
verb_phrase(L0, L2, Ind):-
    verb(L0, L1, Ind),
    prep(L1,L2, Ind), !.

% L0 and L2 make up a difference list that is a legal noun phrase
noun_phrase(L0, L2, Ind) :-
    det(L0, L1, Ind),
    noun(L1, L2, Ind), !.

% creates a list that of action (verb) and object (noun)
make_command_list(V, N, [V|N]).

% execute commands takes the parsed command (verb = action) (noun = thing), and performs such command
execute_command([move|Thing]) :-
    is_direction_move(Thing),
    move(Thing), !.
execute_command([take|Thing]) :-
    take(Thing), !.
execute_command([interact|Person]) :-
    interact(Person, good), !.
execute_command([fight|Person]) :-
    interact(Person, bad), !.
execute_command([bag|_]) :-
    bag, !.
execute_command([describe|_]) :-
    describe, !.
execute_command([die|_]) :-
    die, !.
execute_command([map|_]) :-
    map, !.
execute_command([inspect|Inspectable]) :-
    inspect(Inspectable), !.
execute_command([open|Openable]) :-
    open(Openable), !.
execute_command([check|Item]) :-
    check(Item), !.
execute_command([help|_]) :-
    help,!.
execute_command([rub|Person]) :-
    rub(Person), !.
execute_command(_) :-
    write("There is a time and place for everything, but not now!"), nl.

game_over_command(_) :-
    life_status(dead), nl,
    write("Thanks for playing!").
game_over_command(_) :-
    game_won(yes), nl,
    write("Thanks for playing!").

is_direction_move(north).
is_direction_move(south).
is_direction_move(east).
is_direction_move(west).

det(["the" | L], L,_).
det(["a" | L], L,_).
det(["this" | L], L,_).
det(["that" | L], L,_).
det(L, L,_).

prep(["to"| L], L, _).
prep(["to", "the"| L], L, _).
prep(["into"| L], L, _).
prep(["into"|  L], L, _).
prep(["in", "to"| L], L, _).
prep(["up"| L], L,_).
prep(["up", "to"| L], L,_).
prep(L, L, _).

verb(["die"| L], L, Ind) :- die_verb(Ind).

verb(["help"| L], L, Ind) :- help_verb(Ind).
verb(["map"| L], L, Ind) :- map_verb(Ind).

verb(["move"| L], L, Ind) :- move_verb(Ind).
verb(["go"| L], L, Ind) :- move_verb(Ind).
verb(["walk"| L], L, Ind) :- move_verb(Ind).

verb(["interact"| L], L, Ind) :- interact_verb(Ind). % interactions could be deepened, just create new functions and interactions (fight and talk = diff outcomes!)
verb(["talk"| L], L, Ind) :- interact_verb(Ind).
verb(["approach"| L], L, Ind) :- interact_verb(Ind).
verb(["fight"| L], L, Ind) :- fight_verb(Ind).
verb(["kill"| L], L, Ind) :- fight_verb(Ind).
verb(["threaten"| L], L, Ind) :- fight_verb(Ind).
verb(["hit"| L], L, Ind) :- fight_verb(Ind).
verb(["stab"| L], L, Ind) :- 
    fight_verb(Ind), 
    inventory(CurrentInventory),
    member(item(sword), CurrentInventory).
verb(["stab"| L], L, Ind) :- 
    fight_verb(Ind), 
    inventory(CurrentInventory),
    member(item(magic_sword), CurrentInventory).

verb(["take"| L], L, Ind) :- take_verb(Ind).
verb(["pick", "up"| L], L, Ind) :- take_verb(Ind).
verb(["stache"| L], L, Ind) :- take_verb(Ind).
verb(["get"| L], L, Ind) :- take_verb(Ind).
verb(["grab"| L], L, Ind) :- take_verb(Ind).

verb(["look"| L], L, Ind) :- describe_verb(Ind).
verb(["look", "around" | L], L, Ind) :- describe_verb(Ind).
verb(["describe"| L], L, Ind) :- describe_verb(Ind).

verb(["inspect"| L], L, Ind) :- inspect_verb(Ind).
verb(["look", "at"| L], L, Ind) :- inspect_verb(Ind).

verb(["open"| L], L, Ind) :- open_verb(Ind).

verb(["check"| L], L, Ind) :- check_verb(Ind).

verb(["rub"| L], L, Ind) :- rub_verb(Ind).

noun(["south" | L], L, Ind) :- south_noun(Ind).
% noun(["down" | L], L, Ind) :- south_noun(Ind).
noun(["west" | L], L, Ind) :- west_noun(Ind).
% noun(["left" | L], L, Ind) :- west_noun(Ind).

% EX (could abstract this more)
noun(["heath"| L], L, Ind) :- get_corresponding_direction(start_state, Ind).
noun(["lavender", "heath"| L], L, Ind) :- get_corresponding_direction(start_state, Ind).
noun(["wall"| L], L, Ind) :- get_corresponding_direction(north_state_1, Ind).
noun(["limestone", "wall"| L], L, Ind) :- get_corresponding_direction(north_state_1, Ind).
noun(["cliffs" | L], L, Ind) :- get_corresponding_direction(west_state_1, Ind).
noun(["cliffside" | L], L, Ind) :- get_corresponding_direction(west_state_1, Ind).
noun(["post"| L], L, Ind) :- get_corresponding_direction(west_state_2, Ind).
noun(["trading","post"| L], L, Ind) :- get_corresponding_direction(west_state_2, Ind).
noun(["forest"| L], L, Ind) :- get_corresponding_direction(east_state_1, Ind).
noun(["gravel","path"| L], L, Ind) :- get_corresponding_direction(east_state_1, Ind).
noun(["jungle"| L], L, Ind) :- get_corresponding_direction(south_east_state_1, Ind).
noun(["beach"| L], L, Ind) :- get_corresponding_direction(south_state_1, Ind).
noun(["stormy", "beach"| L], L, Ind) :- get_corresponding_direction(south_state_1, Ind).
noun(["gate"| L], L, Ind) :- get_corresponding_direction(north_state_2, Ind).
noun(["black","gate"| L], L, Ind) :- get_corresponding_direction(north_state_2, Ind).


noun(["east" | L], L, Ind) :- east_noun(Ind).
% noun(["right" | L], L, Ind) :- east_noun(Ind).
noun(["north" | L], L, Ind) :- north_noun(Ind).
% noun(["up" | L], L, Ind) :- north_noun(Ind).

noun(["key" | L], L, Ind) :- key_noun(Ind).
noun(["sword" | L], L, Ind) :- sword_noun(Ind).
noun(["shield" | L], L, Ind) :- shield_noun(Ind).
noun(["map" | L], L, Ind) :- map_noun(Ind).
noun(["boots"| L], L, Ind) :- boots_noun(Ind).
noun(["wizard" | L], L, Ind) :- wizard_noun(Ind).
noun(["zombie" | L], L, Ind) :- zombie_noun(Ind).
noun(["dragon" | L], L, Ind) :- dragon_noun(Ind).
noun(["well" | L], L, Ind) :- well_noun(Ind).
noun(["clam"| L], L, Ind) :- clam_noun(Ind).
noun(["pearl"| L], L, Ind) :- pearl_noun(Ind).
noun(["inventory"| L], L, Ind) :- inventory_noun(Ind).
noun(["strength"| L], L, Ind) :- strength_noun(Ind).
noun(["bag"| L], L, Ind) :- suspicious_bag_noun(Ind).
noun(["rocks"| L], L, Ind) :- pile_of_rocks_noun(Ind).
noun(["pile", "of", "rocks"| L], L, Ind) :- pile_of_rocks_noun(Ind).
noun(["drawer"| L], L, Ind) :- drawer_noun(Ind).
noun(["photo"| L], L, Ind) :- photo_noun(Ind).
noun(["hole"| L], L, Ind) :- hole_in_tree_noun(Ind).
noun(["tree", "hole"| L], L, Ind) :- hole_in_tree_noun(Ind).
noun(["hole", "in", "tree"| L], L, Ind) :- hole_in_tree_noun(Ind).
noun(["driftwood"| L], L, Ind) :- driftwood_noun(Ind).
noun(["sign"| L], L, Ind) :- sign_noun(Ind).
noun(["chest"| L], L, Ind) :- chest_noun(Ind).
noun(["salesman"| L], L, Ind) :- salesman_noun(Ind).
noun(["lamp"| L], L, Ind) :- genie_noun(Ind).

get_corresponding_direction(DestinationState, Direction) :-
    current_state(State),
    path(State, Direction, DestinationState, _).
% we could make it so that we only have 1 move verb clause (the one below)
% and then use that to execute all types of moves (motion, interaction, fight)
% or we could make more clauses (interact_verb, fight_verb, etc.)

die_verb(die).
map_verb(map).
move_verb(move). 
interact_verb(interact).
take_verb(take).
describe_verb(describe). 
% inventory_verb(bag). 
inspect_verb(inspect).
fight_verb(fight).
open_verb(open).
check_verb(check).
help_verb(help).
rub_verb(rub).

north_noun(north).
south_noun(south).
east_noun(east).
west_noun(west).
inventory_noun(bag).

key_noun(item(key)).
sword_noun(item(sword)).
shield_noun(item(shield)).
map_noun(item(gameMap)).
boots_noun(item(boots)).
pearl_noun(item(pearl)).
strength_noun(strength).

wizard_noun(person(wizard)).
zombie_noun(person(zombie)).
dragon_noun(person(dragon)).
genie_noun(person(genie)).
well_noun(person(well)).
clam_noun(inspectable(clam)).
suspicious_bag_noun(inspectable(suspicious_bag)).
pile_of_rocks_noun(inspectable(pile_of_rocks)).
hole_in_tree_noun(inspectable(hole_in_tree)).
driftwood_noun(inspectable(driftwood)).
photo_noun(inspectable(photo)).
sign_noun(inspectable(sign)).
chest_noun(openable(chest)).
drawer_noun(openable(drawer)).
salesman_noun(person(salesman)).

% north_noun(cliffs) :- % here we check if current state is state where north_state=cliffs for example

% describe is used to describe the current state 
describe :-
    life_status(alive),
    game_won(no),
    current_state(State),
    write_state(State), !.
describe :-
    life_status(dead), !. % write nothing if dead
describe :-
    game_won(yes), !. % write nothing if game is won
% write_state writes out the environment of every state to the screen, as well as how they connect to other states
write_state(start_state) :-
    tutorial_needed(Status), (\+ dif(yes, Status)),
    tutorial, nl, nl, nl,
    retract(tutorial_needed(Status)),
    assert(tutorial_needed(no)),
    describe_neighbours, nl,
    % write_state_contents(start_state), nl,
    % exits(start_state), 
    nl.

write_state(_) :-
    describe_current_location, nl,
    describe_neighbours, nl,
    long_describe_contents, nl,
    % write_state_contents(State), nl,
    % exits(State), 
    nl.

% Move from a starting state to a new state, granted a path exists and it is not unlocked
move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, NewState, LockStatus),
    unlock(PreviousState, Move, NewState, LockStatus),
    is_state(NewState), 
    retract(current_state(PreviousState)),
    assert(current_state(NewState)),
    life_status(alive),
    describe, !.

% trying to move to locked place
move(Move) :-
    current_state(PreviousState),
    path(PreviousState, Move, _, LockStatus),
    dif(LockStatus, unlocked),
    \+ unlock(PreviousState, Move, _, LockStatus), % path is NOT unlocked, and we CANNOT unlock it
    getLockWarning(LockStatus, Text),
    write(Text), nl, % in the future, add specific text (for gate and cliffs)
    life_status(alive),
    describe, !.

move(_) :-
    life_status(alive),
    write("Invalid move!"), nl, !.

move(_) :-
    life_status(dead),
    write("Ummm... your dead!"), nl.

% takes Item from current state and puts it into your inventory
take(Item) :-
    current_state(State),
    position(Item, State),
    \+ hidden_by(Item, _),
    input(item(I), Move), 
    retract(position(Item, State)),
    retract(input(item(I), Move)),
    life_status(alive),
    add_to_inventory(Item), describe, !.
take(_) :-
    write("That's an invalid move!"), nl, !.

% check something gives the current status of it
% checkable things are map, inventory, playerstrength
check(item(gameMap)) :-
    map, !.
check(bag) :-
    bag, !.
check(strength) :-
    get_player_strength(Number),
    write("If your current strength was a number, it would be "), write(Number), nl, !.
check(_) :-
    write("That's an invalid move!"), nl, !.

% call with an inspectable thing (thinking something like: inspect(hole_in_tree) --> you see a bracer in the tree someone hid!, then player can say 'take bracer'...)
inspect(Inspectable) :-
    current_state(State),
    position(Item, State),
    hidden_by(Item, Inspectable),
    inspected_text(Inspectable, Text), write(Text), nl,
    retract(hidden_by(Item, Inspectable)), !.
% To inspect inspectables that dont hide items
inspect(Inspectable) :-
    current_state(State),
    position(Inspectable, State),
    inspected_text(Inspectable, Text), write(Text), nl, !.
inspect(_) :-
    write("That's an invalid move!"), nl, !.

% call with person(Person), interact works as default interaction for talk and approach
% INTERACTIONS ARE EITHER GOOD OR BAD
% GOOD INTERACTIONS: talk, approach, call out.
% BAD INTERACTIONS: fight, stab(if have sword), hurt, kill
% type of interaction doesnt matter for dragon or zombie since they are MOBS/inherently bad
interact(Person, TypeOfInteraction) :-
    current_state(State),
    position(Person, State),
    % input(person(P), Move),
    life_status(alive),
    interaction(Person, TypeOfInteraction, RemoveStatus),
    % remove_person(position(Person, State), RemoveStatus),
    remove_person(Person, State, RemoveStatus),
    % add_to_removed_person_list(position(Person, State)),
    % retract(position(Person, State)),
    % retract(input(person(P), Move)), 
    describe, !.
interact(_) :-
    write("That's an invalid move!"), nl, !.

/*
remove_person(PersonPosition, yes) :-
    add_to_removed_person_list(PersonPosition),
    retract(PersonPosition).
*/

remove_person(Person, State, yes) :-
    input(Person, Move),
    add_to_removed_person_list(removed_person_info(Person, State, Move)),
    retract(position(Person, State)),
    retract(input(Person, Move)).

remove_person(_, _, no).
% DRAGON ENCOUNTER
% 5 interactions: 1 win, 1 loss with too little strength, 1 loss with no weapon, 1 loss with basic sword, 1 loss completely
% type of interaction does not matter, he is a dragon, there is no reasoning or talking, just fight!
interaction(person(dragon), _, yes) :-
    inventory(CurrentInventory),
    member(item(magic_sword), CurrentInventory),
    member(item(shield), CurrentInventory),
    get_strength(person(dragon), DragonStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength > DragonStrength,
    write("You repel the dragon's flames with your shield, and pierce the thick scales on his neck with your magic sword!"), nl,
    write("You have beat the adventure game!"), nl,
    game_won(Status),
    retract(game_won(Status)),
    assert(game_won(yes)),
    restart_instructions.

interaction(person(dragon), _, no) :-
    inventory(CurrentInventory),
    member(item(magic_sword), CurrentInventory),
    member(item(shield), CurrentInventory),
    get_strength(person(dragon), DragonStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< DragonStrength,
    write("You repel the dragon's flames with your shield, and trade blows with the dragon!"), nl,
    write("But alas, you are not strong enough! Next time get some more items!"), nl,
    die.

interaction(person(dragon), _, no) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    \+ member(item(magic_sword), CurrentInventory),
    \+ member(item(sword), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But with no weapon, you cannot damage the dragon and grow too tired to fight!"), nl,
    die.

interaction(person(dragon), _, no) :-
    inventory(CurrentInventory),
    member(item(shield), CurrentInventory),
    member(item(sword), CurrentInventory),
    write("You repel the dragon's flames with your shield!"), nl,
    write("But your simple sword can't pierce his tough skin! The dragon won't take any damage!"), nl,
    die.

interaction(person(dragon), _, no) :-
    write("The dragon burns you with his fiery breath!"), nl,
    die.

% SALESMAN interaction
% good interaction - talk to him, he sells you item and runs off happy with a salesman
% bad interaction - you try to kill him, but in doing so you either die or lose your sword.

interaction(person(salesman), good, SaleStatus) :-
    inventory(CurrentInventory),
    member(item(pearl), CurrentInventory),
    say([H|_], "The salesman is selling a 'so-called' magical pendant for 1 pearl, do you want it? " ),
    sale(H, item(pendant), person(salesman), item(pearl), SaleStatus), nl, !.

interaction(person(salesman), good, no) :-
    write("The salesman wants to sell you something, but you have nothing that he wants! He said he likes ocean goods however..."), nl.

interaction(person(salesman), bad, yes) :-
    write("You killed the salesman! [You monster!]"), nl,
    write("Instead of getting his wares, his body disinigrates into dust. You wonder what you missed out on"), nl.

% ZOMBIE ENCOUNTER
% victory - if player is strong enough to defeat zombie (based on contents of inventory),
% then an item in inventory is sacrificed to defeat zombie
interaction(person(zombie), _, yes) :-
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
interaction(person(zombie), _, no) :-
    get_strength(person(zombie), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    write("Oh no! The zombie ate your brain!"), nl,
    die.

% WIZARD ENCOUNTER
% friendly encounter - wizard upgrades the players sword to a magic sword
interaction(person(wizard), good, yes) :-
    inventory(Inventory),
    member(item(sword), Inventory),
    remove_from_inventory(item(sword)),
    add_to_inventory(item(magic_sword)),
    write("The wizard has upgraded your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    nl.
% unfriendly encounter - wizard downgrades the players magic sword to a sword
interaction(person(wizard), bad, yes) :-
    inventory(Inventory),
    member(item(magic_sword), Inventory),
    remove_from_inventory(item(magic_sword)),
    add_to_inventory(item(sword)),
    write("Oh no! The wizard has removed the magic from your sword!"), nl,
    write("How strange: the wizard vanished!"), nl,
    nl.
% unfriendly encounter - wizard attacks if the player doesnt have any sword
interaction(person(wizard), bad, yes) :-
    get_strength(person(wizard), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength > MonsterStrength,
    add_to_inventory(item(gold)),
    write("You tried to attack the wizard but he was nimble and got away. In his escape he dropped some gold!"), nl,
    nl.
interaction(person(wizard), bad, no) :-
    get_strength(person(wizard), MonsterStrength),
    get_player_strength(PlayerStrength),
    PlayerStrength =< MonsterStrength,
    write("Oh no! The wizard turned you into a frog!"), nl,
    die.
interaction(person(wizard), good, yes) :-
    inventory(Inventory),
    \+ member(item(sword), Inventory),
    add_to_inventory(item(gold)),
    write("The wizard flicked a gold coin at you. While you were mesmerized by its shimmer, he vanished!"), nl,
    nl.

% WISHING WELL ENCOUNTER
% friendly encounter - well upgrades the players gold to a magic sword
interaction(person(well), good, no) :-
    inventory(Inventory),
    member(item(gold), Inventory),
    % remove_from_inventory(item(gold)),
    % add_to_inventory(item(magic_sword)),
    % write("On impulse, you toss your sack of gold bullion into the well."), nl,
    % write("The well seems to glow with rainbows. Suddenly you find yourself holding a magic sword!"), nl,
    % write("As the multi-coloured light fades, you notice that the well has vanished."), nl,
    % nl.
    say([H|_], "The well calls out for your gold! Do you toss your Gold Bullion in? " ),
    sale(H, item(magic_sword), person(well), item(gold), _), nl, !.

% friendly encounter - is you have a normal sword, the well summons a wizard
interaction(person(well), good, yes) :-
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
interaction(person(well), good, no) :-
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

open(Openable) :-
    current_state(State),
    been_opened(Openable, no),
    position(Openable, State),
    say(InputPassword, "What is the password? "),
    get_open_items(InputPassword, Openable, State), !.
open(Openable) :-
    current_state(State),
    position(Openable, State),
    been_opened(Openable, yes),
    write("You've already opened this!"), nl, !.
open(_) :-
    write("That's an invalid move!"), nl, !.

get_open_items(InputPassword, Openable, State) :-
    password(Openable, Password),
    same_password(InputPassword, Password),
    position(Item, Openable),
    opened_text(Openable, Text), write(Text), nl,
    retract(position(Item, Openable)),
    assert(position(Item, State)),
    take(Item),
    retract(been_opened(Openable, no)),
    assert(been_opened(Openable, yes)), !.
get_open_items(_,_,_) :-
    write("That didn't work"), nl, !.

same_password([H|[]], Str) :-
    \+ dif(H, Str), !.

sale("yes", Item, Person, TradeItem, yes) :-
    item_name(Item, Name),
    position(Item, Person),
    write("You bought "), write(Name), nl,
    remove_from_inventory(TradeItem),
    add_to_inventory(Item),
    retract(position(Item, Person)),
    sale_text(Person, sold, Text), write(Text), !.

sale("no", _, Person, _, no) :-
    sale_text(Person, unsold, Text), write(Text), nl, !.

sale(_,_,Person, _, no) :-
    sale_text(Person, confused, Text), write(Text), nl.

% A DCG interaction with a genie who loves prolog
% if you share the same opinion about loving prolog (more or equal to haskell), he rewards you!
rub(person(genie)) :-
    write("You rub the lamp and a magical genie appears! He is no ordinary genie however!"), nl,
    write("The genie traps you in a cloud of smoke and says: You do not leave until you answer my question!"), nl,
    genie_interaction.

genie_interaction :-
    repeat,
    say(Input, "The genie says: what is your opinion on haskell and prolog? And respond in a natural, DCG-way I understand!"),
    stringlist_to_atom(Input, AtomInput),
    phrase(genie_opinion(OpinionTree), AtomInput),
    valid_opinion(OpinionTree),
    get_opinion_value(OpinionTree, OpinionValue),
    genie_reaction(OpinionValue), nl,
    current_state(State),
    remove_person(person(genie), State, yes), !.

genie_reaction(1) :-
    write("The genie says: Hate is too strong and evil a feeling to have! I banish you to this lamp forever!"),
    die, !.
genie_reaction(2) :-
    write("The genie says: How could you dislike prolog, its the perfect language! Away from me you cretin!"), nl,
    write("The genie snaps his fingers and teleports you away!"),
    current_state(State),
    retract(current_state(State)),
    assert(current_state(start_state)), !.
genie_reaction(3) :-
    write("The genie says: No opinions on prolog? What a shame!"), nl,
    write("The genie and the lamp vanish!"), !.
genie_reaction(4) :-
    write("The genie says: I like haskell too, but come on! It's not better than prolog!"), nl,
    write("The genie and the lamp vanish!"), !.
genie_reaction(5) :-
    write("The genie says: You only a mere 'like' of prolog! You have clearly not played enough with it!"), nl,
    write("The genie and the lamp vanish!"), !.
genie_reaction(6) :-
    write("The genie says: You noble being, are man of taste and culture! Take this!"), nl,
    position(item(helmet), Person),
    add_to_inventory(item(helmet)),
    retract(position(item(helmet), Person)),
    write("The genie and the lamp vanish!"), !.
genie_reaction(_) :-
    write("The genie says: Hmm... I'm confused"),
    write("The genie and the lamp vanish!").

% Checks to make sure opinion is valid
% valid opinion does not have the same object noun multiple times
% ex. 'I like haskell and I hate haskell', is not valid (in our context)
% ex. 'I love haskell but I love prolog', is not valid either
% single sentence opinions are always valid (1 object noun)
valid_opinion(go(SentenceTree,Conjunction, OpinionTree)) :-
    valid_conjunction_use(SentenceTree, Conjunction, OpinionTree),
    valid_opinion_meaning(SentenceTree, OpinionTree),
    valid_opinion(OpinionTree).
valid_opinion(go(_)). 


% conjunction use is always valid except in the case that but is used with the same verb in each 
% connecting sentence
valid_conjunction_use(SentenceTree, gcon(but), OpinionTree) :-
    get_verb(SentenceTree, Verb1),
    get_first_sentence(OpinionTree, SentenceTree2),
    get_verb(SentenceTree2, Verb2),
    dif(Verb1, Verb2),
    get_next_opinion(OpinionTree, NextOpinion),
    valid_conjunction_use(SentenceTree, gcon(but), NextOpinion).
valid_conjunction_use(SentenceTree, gcon(but), OpinionTree) :-
    get_verb(SentenceTree, Verb1),
    single_sentence_opinion(OpinionTree, SentenceTree2),
    get_verb(SentenceTree2, Verb2),
    dif(Verb1, Verb2).
valid_conjunction_use(_, gcon(morethan), _).
valid_conjunction_use(_, gcon(lessthan), _).
valid_conjunction_use(_, gcon(and), _).

% checks to see if sentence tree and opinion tree has same object noun at all
% also checks if conjunction but is used, and verb is same in each sentence
valid_opinion_meaning(SentenceTree, OpinionTree) :-
    get_noun(SentenceTree, Noun1),
    get_first_sentence(OpinionTree, SentenceTree2),
    get_noun(SentenceTree2, Noun2),
    dif(Noun1, Noun2),
    get_next_opinion(OpinionTree, OpinionTree2),
    valid_opinion_meaning(SentenceTree, OpinionTree2).
valid_opinion_meaning(SentenceTree, OpinionTree) :-
    get_noun(SentenceTree, Noun1),
    single_sentence_opinion(OpinionTree, SentenceTree2),
    get_noun(SentenceTree2, Noun2),
    dif(Noun1, Noun2).

% Mutliple different values of opinion
% Case 1: Tree includes word 'hate'
% all cases after verified to not include 'hate'
get_opinion_value(OpinionTree, OpinionValue) :-
    includes_hate(OpinionTree),
    OpinionValue is 1.
% Case 2: Dislikes Prolog
get_opinion_value(OpinionTree, OpinionValue) :-
    dislike_prolog(OpinionTree),
    OpinionValue is 2.
% Case 3: No mention of prolog (1 sentence haskell opinion)
% after this, verified that opinions include a positive view of prolog (like/love)
get_opinion_value(OpinionTree, OpinionValue) :-
    \+ mention_prolog(OpinionTree),
    OpinionValue is 3.
% Case 4: Liking/Loving Haskell more than Prolog or you love/like prolog less than haskell
get_opinion_value(OpinionTree, OpinionValue) :-
    enjoy_haskell_more_than_prolog(OpinionTree),
    OpinionValue is 4.
% Case 5: Only liking Prolog
get_opinion_value(OpinionTree, OpinionValue) :-
    only_like_prolog(OpinionTree),
    OpinionValue is 5.
% Case 6: Loving Prolog
get_opinion_value(OpinionTree, OpinionValue) :-
    love_prolog(OpinionTree),
    OpinionValue is 6.
get_opinion_value(_, 0).


% GENIE PARSING HELPERS

% true if opinion tree includes loving prolog
love_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree1),
    get_verb(SentenceTree1, Verb),
    \+ dif(gv(love), Verb),
    get_noun(SentenceTree1, Noun),
    \+ dif(gn(prolog), Noun).
love_prolog(OpinionTree) :-
    get_next_opinion(OpinionTree, NextOpinion),
    love_prolog(NextOpinion).

% true if opinion tree includes liking prolog
only_like_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree1),
    get_verb(SentenceTree1, Verb),
    \+ dif(gv(like), Verb),
    get_noun(SentenceTree1, Noun),
    \+ dif(gn(prolog), Noun).
only_like_prolog(OpinionTree) :-
    get_next_opinion(OpinionTree, NextOpinion),
    only_like_prolog(NextOpinion).

% true if opinion tree inlcudes like/loving haskell more than prolog
% conjunction is more than, and first verb is like/love, and first noun is haskell
% conjunction is less than, and first verb is like/love, and first noun is prolog
enjoy_haskell_more_than_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree1),
    get_conjunction(OpinionTree, Conjunction),
    \+ dif(Conjunction, gcon(morethan)),
    get_verb(SentenceTree1, Verb),
    positive_verb(Verb),
    get_noun(SentenceTree1, Noun),
    \+ dif(Noun, gn(haskell)).
enjoy_haskell_more_than_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree1),
    get_conjunction(OpinionTree, Conjunction),
    \+ dif(Conjunction, gcon(lessthan)),
    get_verb(SentenceTree1, Verb),
    positive_verb(Verb),
    get_noun(SentenceTree1, Noun),
    \+ dif(Noun, gn(prolog)).

% true if opinion tree mentions prolog at all
mention_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree),
    get_noun(SentenceTree, Noun),
    \+ dif(gn(prolog), Noun).
mention_prolog(OpinionTree) :-
    get_next_opinion(OpinionTree, NextOpinion),
    mention_prolog(NextOpinion).

% true if opinion tree includes verb hate at all
includes_hate(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree),
    get_verb(SentenceTree, Verb),
    \+ dif(gv(hate), Verb).
includes_hate(OpinionTree) :-
    get_next_opinion(OpinionTree, NextOpinion),
    includes_hate(NextOpinion).

% true if opinion tree includes disliking prolog
dislike_prolog(OpinionTree) :-
    get_first_sentence(OpinionTree, SentenceTree),
    get_verb(SentenceTree, Verb),
    get_noun(SentenceTree, Noun),
    \+ dif(gv(dontlike), Verb), \+ dif(gn(prolog), Noun).
dislike_prolog(OpinionTree) :-
    get_next_opinion(OpinionTree, NextOpinion),
    dislike_prolog(NextOpinion).

positive_verb(gv(love)).
positive_verb(gv(like)).

get_conjunction(go(_,Conjunction,_), Conjunction).
get_next_opinion(go(_,_,OpinionTree), OpinionTree).

get_noun(gs(_, VerbPhrase), Noun) :-
    get_noun_in_vp(VerbPhrase, Noun).
get_verb(gs(_, VerbPhrase), Verb) :-
    get_verb_in_vp(VerbPhrase, Verb).

get_noun_in_vp(gvp(_, NounPhrase), Noun) :-
    get_noun_in_np(NounPhrase, Noun).
get_verb_in_vp(gvp(Verb, _), Verb).

get_noun_in_np(gnp(Noun), Noun).

get_first_sentence(go(SentenceTree, _, _), SentenceTree).
get_first_sentence(go(SentenceTree), SentenceTree).

single_sentence_opinion(go(SentenceTree), SentenceTree).


% GENIE OPINION GRAMMER
% AN OPINION IS: A SENTENCE OR A (SENTENCE + CONJUNCTION + SENTENCE + OPINION)
% A SENTENCE IS: PRONOUN + VERB PHRASE
% A VERB PHRASE IS: VERB + NOUN PHRASE
% A NOUN PHRASE IS: NOUN 
% no determiners or preposition since the topic (haskell and prolog) is simple

genie_opinion(go(GS)) --> genie_sentence(GS).
genie_opinion(go(GS, GCON, GO)) --> genie_sentence(GS), genie_con(GCON), genie_opinion(GO).

genie_sentence(gs(GPRON, GVP)) --> genie_pron(GPRON), genie_vp(GVP).
genie_np(gnp(GN)) --> genie_n(GN).
genie_vp(gvp(GV, GNP)) --> genie_v(GV), genie_np(GNP).

genie_con(gcon(and)) --> [and].
genie_con(gcon(but)) --> [but].
genie_con(gcon(morethan)) --> [more, than].
genie_con(gcon(lessthan)) --> [less, than].
genie_n(gn(haskell)) --> [haskell].
genie_n(gn(prolog))--> [prolog].
genie_pron(gpron(i)) --> [i].

genie_v(gv(like)) --> [like].
genie_v(gv(hate)) --> [hate].
genie_v(gv(love)) --> [love].
% genie_v(gv(dislike)) --> [dislike].
genie_v(gv(dontlike)) --> [dont, like].
% genie_v(gv(dontlove)) --> [dont, love].

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
unlock(PathState, Move, State, UnlockItem) :-
    inventory(CurrentInventory),
    member(UnlockItem, CurrentInventory),
    unlock_item_text(UnlockItem, Text, RemoveAfterUse),
    write(Text), nl,
    remove_lock_item(UnlockItem, RemoveAfterUse),
    retract(path(PathState, Move, State, UnlockItem)),
    assert(path(PathState, Move, State, unlocked)).

remove_lock_item(Item, yes) :-
    remove_from_inventory(Item).
remove_lock_item(_, no).

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

stringlist_to_atom([], []).
stringlist_to_atom([H|T], [A|B]) :-
    string_to_atom(H, A),
    stringlist_to_atom(T, B).

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
% reset map to south_east_state_1
reset_state_items([H|T]) :-
    \+ dif(H, item(gameMap)),
    \+ (position(H, south_east_state_1)),
    assert(position(H, south_east_state_1)),
    assert(input(H, a)),
    reset_state_items(T).
% reset boots to north_state_1
reset_state_items([H|T]) :-
    \+ dif(H, item(boots)),
    \+ (position(H, north_state_1)),
    assert(position(H, north_state_1)),
    assert(input(H, a)),
    reset_state_items(T).
% reset pendant to salesman
reset_state_items([H|T]) :-
    \+ dif(H, item(pendant)),
    \+ (position(H, person(salesman))),
    assert(position(H, person(salesman))),
    reset_state_items(T).
% reset helmet to genie
reset_state_items([H|T]) :-
    \+ dif(H, item(helmet)),
    \+ (position(H, person(genie))),
    assert(position(H, person(genie))),
    reset_state_items(T).
% reset armour to chest
reset_state_items([H|T]) :-
    \+ dif(H, item(armour)),
    \+ (position(H, openable(chest))),
    assert(position(H, openable(chest))),
    reset_state_items(T).
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

add_to_removed_person_list(removed_person_info(person(P), State, Move)) :-
    removed_person_list(Old_person_list),
    list_add(removed_person_info(person(P), State, Move), Old_person_list, New_person_list),
    retract(removed_person_list(Old_person_list)),
    assert(removed_person_list(New_person_list)).
    
% return wizard / zombie / dragon / etc. to start position at start of each game
return_characters_from_removed_person_list([removed_person_info(person(P), State, Option)|T]) :-
    assert(position(person(P), State)),
    %get_next_option_index(State, Option),
    assert(input(person(P), Option)),
    return_characters_from_removed_person_list(T).
return_characters_from_removed_person_list([]).

% next option
next_option(a,b).
next_option(b,c).
next_option(c,d).
next_option(d,e).
/*
get_next_option_index(State, Option) :-
    position(I, State),
    increment_contents([I], a, Option). %, fail.
increment_contents([_|T], CurrentOption, FinalOption) :-
    next_option(CurrentOption, NextOption),
    increment_contents(T, NextOption, FinalOption).
increment_contents([], CurrentOption, FinalOption) :-
    next_option(CurrentOption, FinalOption).
*/
% assumes options will not overfill
get_next_option_index(State, Option) :-
    increment_contents(a, State, Option).
increment_contents(CurrentOption, State, FinalOption) :-
    input_taken(CurrentOption, State),
    next_option(CurrentOption, NextOption),
    increment_contents(NextOption, State, FinalOption).
increment_contents(CurrentOption, State, CurrentOption) :-
    \+ input_taken(CurrentOption, State),!.

input_taken(Option, State) :- position(I, State), input(I, Option).

exits(State) :-
    path(State, Move, Exit, LockStatus),
    write_exits(Move, Exit, LockStatus), fail.
exits(_).

write_exits(Move, Exit, unlocked) :-
    state_name(Exit, Name),
    write("Type "), write(Move), write(" to go to "), write(Name), nl.

write_exits(Move, Exit, UnlockItem) :-
    state_name(Exit, Name),
    item_name(UnlockItem, ItemName),
    write("Type "), write(Move), write(" to  "), write(Name), write(" [LOCKED, REQUIRES: "), write(ItemName), write(" ]"), nl.


tutorial :-
    write("Welcome to the game, this is a text-based adventure game!"), nl,
    write("Current basic rules are as follows:"), nl,
    write("Use natural language to traverse this weird world and find out what your goal is!"), nl,
    write("Command using lowercase letters only!"), nl,
    write("Type 'help.' for more help on commands"), nl, nl, nl, nl,
    write("You wake up in the middle of a peaceful, lavender heath surrounded by forest and cliffs. Some may even call it the central heath."), nl.
help :-
    write("Here is a list of all commands: "), nl,
    findall(C, verb([C|_],_,_), L),
    writeall(L).

writeall([]).
writeall([H|T]) :-
    write(H), nl,
    writeall(T).
% ITEMS
% Items their position in start of game. Currently only in states, future could be in chests, boxes, given to player by NPC etc.
% may change formatting so that its: item(name, property, value), (ex, item(sword, position, east_state_1)) 

% MONSTERS
% monsters placed at start of game, later move??????????

:-dynamic(position/2).
:-dynamic(input/2).
:-dynamic(hidden_by/2).
:-dynamic(been_opened/2).
position(item(sword), east_state_1).
position(item(shield), east_state_1).
position(item(key), west_state_1).
position(item(gameMap), south_east_state_1).
position(item(boots), north_state_1).
position(item(armour), openable(chest)).
position(item(ring), openable(drawer)).
position(item(pendant), person(salesman)).
position(item(pearl), south_state_1).
position(item(magic_sword), person(well)).
position(item(helmet), person(genie)).
position(openable(chest), south_east_state_1).
position(openable(drawer), west_state_2).
position(inspectable(hole_in_tree), south_east_state_1).
position(inspectable(driftwood), south_state_1).
position(inspectable(photo), west_state_2).
position(inspectable(sign), north_state_1).
position(person(well), south_state_1).
position(person(dragon), north_state_2).
position(person(zombie), east_state_1).
position(person(wizard), west_state_1).
position(person(wizard), south_east_state_1).
position(person(salesman), west_state_2).
position(person(genie), south_state_1).
input(item(sword), a).
input(item(shield), b).
input(item(key), a).
input(item(gameMap), a).
input(item(boots), a).
input(openable(chest), b).
input(openable(drawer), d).
input(inspectable(hole_in_tree), d).
input(inspectable(driftwood), d).
input(inspectable(photo), b).
input(inspectable(sign), b).
input(item(pearl), b).
input(person(dragon), a).
input(person(zombie), c).
input(person(wizard), c).
input(person(well), a).
input(person(salesman), a).
input(person(genie), e).
item_name(item(sword), 'Sword').
item_name(item(shield), 'Shield').
item_name(item(key), 'Key').
item_name(item(magic_sword), 'Magic Sword').
item_name(item(gold), 'Gold Bullion').
item_name(item(gameMap), 'Map').
item_name(item(boots), 'Hiking Boots').
item_name(item(armour), 'Ancient Armour').
item_name(openable(chest), 'Ancient Chest').
item_name(openable(drawer), "Drawer").
item_name(item(pendant), 'Magical Pendant').
item_name(item(pearl), 'Pearl').
item_name(item(ring), 'Ruby Ring').
item_name(item(helmet), 'Genie Helmet').
inspectable_name(inspectable(suspicious_bag), "Suspicious Bag").
inspectable_name(inspectable(pile_of_rocks), "Pile of Rocks").
inspectable_name(inspectable(hole_in_tree), "Hole in Tree").
inspectable_name(inspectable(driftwood), "Driftwood").
inspectable_name(inspectable(clam), "Clam").
inspectable_name(inspectable(photo), "Photo").
insepctable_name(inspectable(sign), "Sign").
person_name(person(dragon), "Boss Dragon").
person_name(person(zombie), "Zombie").
person_name(person(wizard), "Wizard").
person_name(person(well), 'Magic Well').
person_name(person(salesman), "Travelling Salesman").
person_name(person(genie), "Genie").

password(openable(chest), "sesame").
password(openable(drawer), "haskell").
been_opened(openable(chest), no).
been_opened(openable(drawer), no).
opened_text(openable(chest), "You opened the chest! Inside you found a full set of Ancient Armour!").
opened_text(openable(drawer), "You opened the drawer! Inside you found a ruby ring glowing with power!").

sale_text(person(salesman), sold, "The salesman sings and dances and runs out the backdoor in glee. You hope you weren't just scammed.").
sale_text(person(salesman), unsold, "The salesman says you're missing out. Are you?").
sale_text(person(salesman), confused, "The salesman is confused. Maybe do something simpler.").
sale_text(person(well), sold, "The well seems to glow with rainbows. Suddenly you find yourself holding a magic sword!").
sale_text(person(well), unsold, "The well lets out a sad echo.").
sale_text(person(well), confused, "The well lets out a confused echo.").

unlock_item_text(item(key), "You open the black gate and proceed!", yes).
unlock_item_text(item(boots), "You're able to traverse up the steep cliffs!", no).

getLockWarning(item(key), "The gate is locked and requires a big key!").
getLockWarning(item(boots), "The cliffside is too steep and requires climbing boots!").

hidden_by(item(boots), inspectable(suspicious_bag)).
hidden_by(item(key), inspectable(pile_of_rocks)).
hidden_by(item(pearl), inspectable(clam)).
inspected_text(inspectable(suspicious_bag), "You unsheath the bag to reveal a pair of boots!").
inspected_text(inspectable(pile_of_rocks), "You walk closer to see that the glimmer of gold is actually a key!").
inspected_text(inspectable(hole_in_tree), "You look in the hole to see nothing but cobwebs and dust. You feel disappointed.").
inspected_text(inspectable(driftwood), "On closer look you see the word 'sesame' etched all over it.").
inspected_text(openable(chest), "You see there is no key hole and you can't open it. Maybe its voice activated?").
inspected_text(openable(drawer), "It is locked with a wordlock. Maybe there's a clue in this room?").
inspected_text(inspectable(clam), "You open up the clam to find a pearl!").
inspected_text(inspectable(photo), "On closer look, its a photo of a man programming. Behind him a sign that says, 'THE BEST FUNCTIONAL LANGUAGE IS...', but you can't make out the rest").
inspected_text(inspectable(sign), "The sign is charred with fire. All you can make out are the words, 'GREATER THAN 11' ").

description_long(item(sword), "a tiny, rusted sword. You don't think about the one who dropped it").
description_long(item(shield), "a heavy shield, cracked and bloodstained. You REALLY don't want to think about who dropped it.").
description_long(item(key), "a large key someone must have forgotten.").
description_long(item(magic_sword), "a greatsword, massive and glowing with epic glory.").
description_long(item(gold), "a large, stained sack of gold.").
description_long(item(gameMap), "a large, crumbling parchment map.").
description_long(item(boots), " a heavy pair of leather boots, perfect for scaling cliffs!").
description_long(item(pendant), " a heavy, ruby pendant that's warm to the touch.").
description_long(item(pearl), " a lovely, platinum pearl. This has gotta be worth something to somebody!").
description_long(item(helmet), " a sparkling helmet lined with colorful jewels!").
description_long(person(dragon), "a massive, red dragon. He's busy with his lunch so he doesn't notice you at first.").
description_long(person(zombie), "a zombie drooling brains.").
description_long(person(wizard), "a wizard is practicing his spells, shooting violent black and purple zaps of lightening.").
description_long(person(well), "a magic well that seems to be whispering your destiny to you.").
description_long(person(salesman), "a salesman with a large trenchcoat full of items. He whistles a jaunty tune.").
description_long(person(genie), "a saffron, gold lamp that seems to be ever so slightly moving.").
description_long(inspectable(suspicious_bag), "a suspicious looking cloth bag that flaps in the wind.").
description_long(inspectable(pile_of_rocks), "a pile of rocks, with a little glimmer of gold.").
description_long(inspectable(hole_in_tree), "a large hole in the center of an old jungle tree.").
description_long(inspectable(driftwood), "a long piece of driftwood covered in etchings.").
description_long(inspectable(clam), "a huge, grey clam.").
description_long(inspectable(photo), "a small grainy photo in a cracked frame").
description_long(inspectable(sign), "a little, crooked sign on a post").
description_long(openable(chest), "a large ancient chest wrapped in roots and vines.").
description_long(openable(drawer), "a splintered wooden drawer with all but 1 of its drawers missing").

get_player_strength(PlayerStrength) :-
    inventory(Inventory),
    get_sum_inventory_strength(PlayerStrength, Inventory).
get_sum_inventory_strength(0, []).
get_sum_inventory_strength(Strength, [H|T]) :-
    get_strength(H, ItemStrength),
    get_sum_inventory_strength(ListStrength, T),
    Strength is ListStrength + ItemStrength.
get_strength(item(sword), 3) :- !.
get_strength(item(shield), 2) :- !.
% get_strength(item(key), 0).
get_strength(item(magic_sword), 6) :- !.
get_strength(item(gold), 1) :- !.
get_strength(item(armour), 3) :- !.
get_strength(item(boots), 1) :- !.
get_strength(item(ring), 1) :- !.
% get_strength(item(gameMap), 0).
get_strength(item(pendant), 2) :- !.
get_strength(item(helmet), 2) :- !.
get_strength(item(_), 0).
get_strength(person(zombie), 4).
get_strength(person(dragon), 4).
get_strength(person(wizard), 4).
get_strength(person(dragon), 11).

% LOCATION STUFF

:-dynamic(path/4).
% paths describe relation between Current_State, move, next_state, and next_state_lock_status
% lock status is either: unlocked, or an item that is required to unlock the state


shuffle_map :-
    remove_all_states,
    % add the inner 4 states around the base
    random(0, 24, Random),
    assign_4_random_nums(Random, R1, R2, R3, R4),
    state_index(State1, R1),
    state_index(State2, R2),
    state_index(State3, R3),
    state_index(State4, R4),
    assert(path(start_state, east, State1, unlocked)),
    assert(path(State1, west, start_state, unlocked)),
    assert(path(start_state, west, State2, unlocked)),
    assert(path(State2, east, start_state, unlocked)),
    assert(path(start_state, north, State3, unlocked)),
    assert(path(State3, south, start_state, unlocked)),
    assert(path(start_state, south, State4, unlocked)),
    assert(path(State4, north, start_state, unlocked)),
    % add outer three states
    random(0, 4, R5),
    add_endgame_states(R5),
    add_locked_state(R5).

assign_4_random_nums(0, 0, 1, 2, 3).
assign_4_random_nums(1, 0, 1, 3, 2).
assign_4_random_nums(2, 0, 2, 1, 3).
assign_4_random_nums(3, 0, 2, 3, 1).
assign_4_random_nums(4, 0, 3, 1, 2).
assign_4_random_nums(5, 0, 3, 2, 1).
assign_4_random_nums(6, 1, 0, 2, 3).
assign_4_random_nums(7, 1, 0, 3, 2).
assign_4_random_nums(8, 1, 2, 0, 3).
assign_4_random_nums(9, 1, 2, 3, 0).
assign_4_random_nums(10, 1, 3, 0, 2).
assign_4_random_nums(11, 1, 3, 2, 0).
assign_4_random_nums(12, 2, 1, 0, 3).
assign_4_random_nums(13, 2, 1, 3, 0).
assign_4_random_nums(14, 2, 0, 1, 3).
assign_4_random_nums(15, 2, 0, 3, 1).
assign_4_random_nums(16, 2, 3, 1, 0).
assign_4_random_nums(17, 2, 3, 0, 1).
assign_4_random_nums(18, 3, 1, 2, 0).
assign_4_random_nums(19, 3, 1, 0, 2).
assign_4_random_nums(20, 3, 2, 1, 0).
assign_4_random_nums(21, 3, 2, 0, 1).
assign_4_random_nums(22, 3, 0, 1, 2).
assign_4_random_nums(23, 3, 0, 2, 1).
    
add_endgame_states(DirectionNum) :-
    direction_index(Direction, DirectionNum),
    direction_oppsite(Direction, OppositeDirection),
    path(start_state, Direction, State1, unlocked),
    assert(path(State1, Direction, north_state_1, unlocked)),
    assert(path(north_state_1, OppositeDirection, State1, unlocked)),
    assert(path(north_state_1, Direction, north_state_2, item(key))),
    assert(path(north_state_2, OppositeDirection, north_state_1, unlocked)), !.
add_locked_state(OppositeDirectionNum) :-
    direction_index(OppositeDirection, OppositeDirectionNum),
    direction_oppsite(Direction, OppositeDirection),
    path(start_state, Direction, State1, unlocked),
    assert(path(State1, Direction, west_state_1, item(boots))),
    assert(path(west_state_1, OppositeDirection, State1, unlocked)), !.

remove_all_states :-
    path(A, _, B, _),
    retract(path(A, _, B, _)),
    remove_all_states, fail.
remove_all_states.

path(start_state, east, east_state_1, unlocked).
path(east_state_1, west, start_state, unlocked).
path(start_state, west, west_state_1, item(boots)).
path(west_state_1, east, start_state, unlocked).
path(start_state, north, north_state_1, unlocked).
path(north_state_1, north, north_state_2, item(key)). % game finale state
path(north_state_2, south, north_state_1, unlocked).
path(north_state_1, south, start_state, unlocked).
path(east_state_1, south, south_east_state_1, unlocked).
path(south_east_state_1, north, east_state_1, unlocked).
path(south_state_1, east, south_east_state_1, unlocked).
path(south_east_state_1, west, south_state_1, unlocked).
path(west_state_1, west, west_state_2, unlocked).
path(west_state_2, east, west_state_1, unlocked).

is_state(start_state).
is_state(east_state_1).
is_state(west_state_1).
is_state(north_state_1).
is_state(north_state_2).
is_state(south_east_state_1).
is_state(south_state_1).
is_state(west_state_2).

state_index(east_state_1, 0).
state_index(south_east_state_1, 1).
state_index(south_state_1, 2).
state_index(west_state_2, 3).
state_index(west_state_1, 4).
state_index(north_state_1, 5).
state_index(north_state_2, 6).
state_index(start_state, 7).

direction_index(north, 0).
direction_index(east, 1).
direction_index(south, 2).
direction_index(west, 3).
direction_index(north, 4).
direction_index(east, 5).
direction_index(south, 6).
direction_index(west, 7).

direction_oppsite(north, south).
direction_oppsite(south, north).
direction_oppsite(east, west).
direction_oppsite(west, east).

state_name(west_state_1, "cliffs").
state_name(start_state, "lavender heath").
state_name(east_state_1, "dark forest path").
state_name(north_state_1, "black gate").
state_name(north_state_2, "open gate").
state_name(south_east_state_1, "ominous jungle").
state_name(south_state_1, "stormy seaside").
state_name(west_state_2, "trading post").

% describing the location from the next location over
state_neighbour_description(west_state_1, "you see zaps of bright lights breaking from atop the cliffside.").
state_neighbour_description(start_state, "the peaceful, lavender heath.").
state_neighbour_description(east_state_1, "a worn down gravel path, darkened by the canopy of the forest.").
state_neighbour_description(north_state_1, "a massive limestone wall with a large black gate.").
state_neighbour_description(north_state_2, "behind the gate you see plumes of dark smoke rising into the air.").
state_neighbour_description(south_east_state_1, "a jungle where you hear the terrible screams of jaguars and flesh-eating parrots.").
state_neighbour_description(south_state_1, "you see a stormy beach, littered in jagged rocks.").
state_neighbour_description(west_state_2, "a tiny, wodden trading post with a sign that says open.").

% describe the current location
state_current_description(west_state_1, "You are on atop the cliffside. The land is barren and wind is fierce.").
state_current_description(start_state, "You stand in the center of a lavender heath surrounded by cliffs and forest.").
state_current_description(east_state_1, "You are in the middle of the forest. It's damp, dark, you can barely see. Your feet sink into the mossy dirt beneath you.").
state_current_description(north_state_1, "You are at the foot of the black gate. The limestone wall goes on for what seems like forever. The ground rumbles below your feet.").
state_current_description(north_state_2, "You are in some sort of dragon nest. Carcasses and old scales scatter the ground.").
state_current_description(south_east_state_1, "You are in a jungle. Parrots with sharp beaks stare down at you, and all around brightly coloured carnivorous flowers seem to smile at you.").
state_current_description(south_state_1, "You are on a windswept beach. The waves crash on the shoreline with a deafening roar, soaking you in freezing seaspray.").
state_current_description(west_state_2, "You are in a small, 1-room wooden cabin. The windows have been smashed, and you're shrouded in the scent of mildew.").

% describing directions to locations
direction_description(east, "To the east, ").
direction_description(north, "To the north, ").
direction_description(west, "To the west, ").
direction_description(south, "To the south, ").

% describe placement of items, thematically related to location
item_prefix(west_state_1, a, "Beside you ").
item_prefix(west_state_1, b, "At the opposite end of the cliffside ").
item_prefix(west_state_1, c, "In a small damp cave, you see ").
item_prefix(west_state_1, d, "Hanging off a small windswept tree with scars from lightning strikes ").
item_prefix(west_state_1, e, "Half-buried under a small rockslide is ").
item_prefix(west_state_2, a, "On a raggety carpet sits ").
item_prefix(west_state_2, b, "On the right wall there is ").
item_prefix(west_state_2, c, "In the darkest corner you see ").
item_prefix(west_state_2, d, "To the left you see ").
item_prefix(west_state_2, e, "Through the back door, on the back porch there is ").
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
item_prefix(south_east_state_1, b, "Beyond a piranha-infested stream, you see ").
item_prefix(south_east_state_1, c, "Lurking in the shadows, you notice ").
item_prefix(south_east_state_1, d, "To your right, ").
item_prefix(south_east_state_1, e, "To your left, ").
item_prefix(south_state_1, a, "Half-buried in sand, there is ").
item_prefix(south_state_1, b, "Sitting in the shallows, you see ").
item_prefix(south_state_1, c, "Bobbing in the waves, you notice ").
item_prefix(south_state_1, d, "Between some rocky spires, ").
item_prefix(south_state_1, e, "At the high-tide line, ").

long_describe_contents :-
    current_state(State),
    position(I, State),
    long_describe_item([I], State), fail.
long_describe_contents.

long_describe_item([], _).
long_describe_item([H|T], State) :-
    input(H, InputIndex),
    \+ hidden_by(H, _),
    item_prefix(State, InputIndex, DescriptionPart1),
    description_long(H, DescriptionPart2),
    write(DescriptionPart1), write(DescriptionPart2), nl,
    long_describe_item(T, State).
long_describe_item([H|T], State) :-
    input(H, InputIndex),
    hidden_by(H, Inspectable),
    item_prefix(State, InputIndex, DescriptionPart1),
    description_long(Inspectable, DescriptionPart2),
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


