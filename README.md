# Loading Planes Simulation (Haskell)

The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation.

## Parameters of problem

A reasonable first representation is the plane as a tile-based map with passengers occupying one tile each.

### Passengers

There will be 100 passengers. People with special loading needs (e.g. infants, the disabled) will be disregarded for simplification. Likewise, berks who get into the wrong seat, or change their minds about stowing their carry-on, and any other special cases will be disregarded.

The passengers will step in turn towards their seat and time will be the total number of steps required (sum over all passengers). Turn-based movement is actually not a bad approximation, since in the real world passengers can only move forward after the person in front is out of the way.

My first attempt at the algorithm controlling passengers' movement is as follows:
+ Passengers get a chance to step in turn.
+ Each passenger uses Dijkstra's algorithm to find their seat (overkill?).
+ UPDATE: Shuffled Dijkstra's algorithm into a submodule and swapped for a very basic move-X-until-correct-repeat-for-Y algorithm.

Initial programming simplifications that should be revisited:
- Stowing luggage prior to getting into seat will be neglected.
- I will assume that a seated passenger does not occupy a tile.
- Politeness will not be considered.
- Skinny people squeezing past other skinny people will not be considered.

### Plane
I've chosen to approximate a jumbo jet (something like a 777), since these seem most inefficient to load. In such a plane there are 4 seats in the middle, and 3 seats on each side making a total of 10 per row. A reasonable simplification is to only model half the plane, i.e. 5 seats A-E.

    /////////////windows/////////////////////
    A ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
    B ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
    C ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
    -> passengers in
    D ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
    E ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
    /////////////mirror//////////////////////

This is because immediately after embarking passengers are directed to the relevant side of the plane, so there is almost never any crossover from the left side to the right. The two sides of the plane are essentially mirror images so it's not necessary to model both.

20 rows will be modelled, so it assumed that all 100 passengers are present and no-one is still in the airport bar. The aisle will be the width of 1 passenger, as is generally the case in real life.

### Visualization

Dipping my toes into HOpenGL to render the plane. OpenGL is rather overkill, given that I'm only going to represent it as 2D tiles, but it's interesting to see how such imperative rendering is done in Haskell.
