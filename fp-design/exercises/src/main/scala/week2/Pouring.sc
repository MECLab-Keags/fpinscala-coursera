
class Pouring(capacity: Vector[Int]) {

    type State = Vector[Int]
    val initialState = capacity map (x => 0)

    trait Move {
        def change(state: State) : State
    }
    case class Empty(glass: Int) extends Move {
        def change(state: State) = state updated (glass, 0)
    }
    case class Fill(glass: Int) extends Move {
        def change(state: State) = state updated (glass, capacity(glass))
    }
    case class Pour(from: Int, to: Int) extends Move {
        def change(state: State) = {
            val amount = state(from) min (capacity(to) - state(to))
            state updated (from, state(from) - amount) updated (to, state(to) + amount)
        }
    }

    val glasses = 0 until capacity.length

    val moves =
        (for (g <- glasses) yield Empty(g)) ++
        (for (g <- glasses) yield Fill(g))  ++
        (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

    class Path(history: List[Move]) {
        lazy val endState: State = history.foldRight(initialState)(_ change _)
        def extend(move: Move) = new Path(move :: history)

        override def toString = (history.reverse mkString " -> ") + " --> " + endState + "\n"
    }

    val initialPath = new Path(Nil)
    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
        if(paths.isEmpty) Stream.empty
        else {
            val more = for {
                path <- paths
                next <- moves map path.extend
                if !(explored contains next.endState)
            } yield next

            paths #:: from(more, explored ++ (more map (_.endState)))
        }

    val pathSets = from(Set(initialPath), Set(initialState))

    def solutions(target: Int): Stream[Path] =
        for{
            pathSet <- pathSets
            path <- pathSet
            if(path.endState contains target)
        } yield path
}

val problem = new Pouring(Vector(4, 7))
problem.moves
problem.solutions(6)

val dieHard = new Pouring(Vector(3,5))
dieHard.solutions(4).take(3).toList

val problem2 = new Pouring(Vector(4, 9, 19))
problem2.solutions(17)
//problem.pathSets.take(3).toList