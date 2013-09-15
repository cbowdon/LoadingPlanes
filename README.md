# Loading Planes Simulation (Haskell)

The great quest to master Haskell/tackle interesting problems continues. I vaguely remember seeing a news article about simulations showing the most efficient way to load passengers onto a plane. As I recall, letting people on at random is least efficient, loading back-to-front somewhat efficient and loading windows-to-aisles most efficient. This project is a recreation of that simulation. The passengers will step in turn towards their seat and time will be the total number of steps required.

Clearly a pathfinding algorithm is required here, so let's get that out of the way first. Wikipedia suggests Dijkstra's algorithm (or some variant) so let's start with that.
