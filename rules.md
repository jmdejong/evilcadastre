
# Evil Cadastre concept rules

The playing field is a large grid of characters (eg 256x256).

Once a day, a cron script will run to update the field based on the old state and on the player inputs.

Player inputs passed by scanning a file somewhere within a user's homedir (eg ~/.cadastre/evil/moves.txt)

The possible inputs are buildings to build somewhere.

## Buildings

### Outpost/Town Center/Tower

I'm not sure about the name, but I'll use outpost for now.

Allows the player to built within a certain distance (manhattan distance) of this building.

Players are not allowed to build outside of the range of the outpost. (except if they have nothing. Then they can build an initial outpost)

If outposts of multiple players cover some area, a tiebreaker is needed (currently undecided).

Maybe there should be different types of outposts, with a different building distance.

A game where I've seen this concept in use is Anno 1503.


### Farm/Factory

Farms are used to get resources.

Resources determine how many things the player can build in the next turn.
Some buildings also need resources for maintenance.

Resources can not be saved over multiple turns.

### Cannon

There are 4 types of cannons, one for each direction.

Each turn, a cannon fires in a direction and destroys the first building there (within a maximum distance).

Multiple cannons can be placed behind each other to destroy multiple buildings.

Cannons will require resources each turn.

Any attempts do build in a cannon's shot fails.

### Minion Post

Minion posts will destroy the nearest enemy building within a certain distance
Minion posts will require resources as well


### Wall

Can protect buildings from cannons (and maybe also minion posts)

### Granary/Storage/Treasury?

Granaries could save leftover resources from some turn to be used in a later turn

### Mint?

Generates tcoin if held for a longer time



## GamePlay

This grid is initially empty (filled with some background characters)

Players who have no presence on the map can make one outpost on any unclaimed place.

Either this first outpost doubles as farm, or the first farms are free (haven't decided yet).

Slowly they will accumulate resources to build more outposts and weaponry.

The larger the player's empire grows, the harder it will be to keep it.
Corruption, bureacracy and popular resistance will increase costs and decrease production.
Maybe revolutions will even happen causing the player control of some parts
This should probably happen based on the integral of the player's influence, so it will get harder and harder and all empires will eventually fall (making room for new players).


## Examples

Map keys:

`,` Empty unclaimed land
`.` Empty claimed land
`O` Outpost
`=` Farm
`g` Granary
`<` Cannon
`>` Cannon
`^` Cannon
`v` Cannon
`M` Minion post
`#` Wall

These map keys are still undecided. Many of the parameters (costs, production, distances) are undecided as well.

### Starting

The initial state of an 8x8 map will look like this (coordinates added for readability):
    
      01234567
    0 ,,,,,,,,
    1 ,,,,,,,,
    2 ,,,,,,,,
    3 ,,,,,,,,
    4 ,,,,,,,,
    5 ,,,,,,,,
    6 ,,,,,,,,
    7 ,,,,,,,,

Now a player can have this input:

    3 3 O

Which means "Build an outpost at (3,3)"

(the player couldn't really do anything else since they haven't claimed any land)

After the next update, the map will look like this:
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,.....,,
    3 ...O...,
    4 ,.....,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,

A player will always get at least 2 resources during a turn (from hunting/gathering or something)

    resources: player1: 2

The player can build a farm for 2 resources each:

    3 2 =

Which is built on the next turn:

      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,.....,,
    3 ..=O...,
    4 ,.....,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,
    
    resources: player1: 2

The farm is producing one resource, and the other resource is gathered.

The number of available resources is now defined by the formula `min(#farms, 2)` (where `#farms` is the number of farms on the map)

The player builds another farm:

    3 2 =
    2 3 =

Next turn:
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,..=..,,
    3 ..=O...,
    4 ,.....,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,
    
    resources: player1: 2


Now the player can start making more farms:

    3 2 =
    2 3 =
    4 3 =
    3 4 =

Building things that are already there won't cost resources (except if the buildings need mainenance)
There are not enough resources to build the last farm though, so it will be ignored for this round.
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,..=..,,
    3 ..=O=..,
    4 ,.....,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,
    
    resources: player1: 3


The next round the player will have enough resources, so the same input will build the next farm
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,..=..,,
    3 ..=O=..,
    4 ,..=..,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,
    
    resources: player1: 4

Note that the 1 unused resource from the last round is discarded

To save leftover resources for the next round player can build granaries.
Granaries cost 4 resources, but will give these resources back the next turn, after which they will be destroyed:

    3 3 O
    3 2 =
    2 3 =
    4 3 =
    3 4 =
    4 4 g

Next turn:
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,..=..,,
    3 ..=O=..,
    4 ,..=g.,,
    5 ,,...,,,
    6 ,,,.,,,,
    7 ,,,,,,,,
    
    resources: player1: 8

The player now has 8 resources, which is enough to build another outpost, which means more buildable land will become available:


    3 3 O
    3 2 =
    2 3 =
    4 3 =
    3 4 =
    4 5 O
    
next round:
    
      01234567
    0 ,,,.,,,,
    1 ,,...,,,
    2 ,..=..,,
    3 ..=O=..,
    4 ,..=...,
    5 ,...O...
    6 ,,.....,
    7 ,,,...,,
    
    resources: player1: 3

Outposts beyond the first one will require resoureces for maintenance.
The number of resources is now given by the formula `max(#farms + 3 * #granaries, 2) - max(#outposts - 1, 0)`.

### Combat

TODO


