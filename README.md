# SCheMeRogueLike

This is my personal attempt to realize a roguelike with Chicken Scheme.
The game is not playable yet.


## Dependencies

* Chicken Scheme (at the moment it works only with the interpreter, but it will be compilable)
* Some SRFIs that are included in Chicken Scheme
* *SRFI 25* (installable like an egg)
* The egg *random-bsd* for the pseudorandom numbers generation
* The egg *numbers* for the exact rational numbers representation
* The egg *ncurses* to interface with the terminal
* The egg *coops* to have an object oriented system

## Files
### Dijkstra.scm
It contains the algorithms to create the Dijkstra maps and estabilish the shortest path.

### Dungeon.scm
It provides the dungeon generation, this is the algorithm general explanation:

1. Generate a grid with every cell set to wall
2. Dig some random rooms with random size (overlapping is not allowed)
3. Dig a maze with a recursive backtracking algorithm (now the rooms are not connected between them nor with the maze but the maze is connected in a topological meaning)
4. For each room dig a passage to the maze
5. Bury the corridor's dead ends for a prefixed number of cell
5. Lighten the dungeon randomly burying some cells in order to create new passages

### Fov.scm
It implements a precise shadowcasting to calculate the field of vision.

### Scheduler.scm
It implements a scheduler which is able to manage different events with different lengths

### Terminal.scm
It provides the interface (input and output) with the terminal, it uses intensively the nCurses library.


## Important
There might be some graphical glitches, to avoid them it's necessary to set the environment variable *TERM* to *vte*.

`export TERM=vte`
