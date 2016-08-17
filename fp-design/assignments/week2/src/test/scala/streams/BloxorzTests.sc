import streams.{GameDef, Solver, StringParserTerrain}

trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
    ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
    }
    }
}

trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
        """ooo-------
          |oSoooo----
          |ooooooooo-
          |-ooooooooo
          |-----ooToo
          |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
}

val t1 = new Level1 {

}

t1.startBlock

val t1Hist = { for {
    ns <- {
        t1.neighborsWithHistory(t1.startBlock, Nil)
    }
    in <- ns._1.legalNeighbors
} yield (in._1, ns._2.union(List(in._2))) }.toList
val t1From = t1.from(Stream((t1.startBlock, Nil)), Set.empty) take(10) toList


/*
List(
    (Block(Pos(1,1),Pos(1,1)),List(Right, Left)),
    (Block(Pos(1,4),Pos(1,4)),List(Right, Right)),
    (Block(Pos(2,2),Pos(2,3)),List(Right, Down)),
    (Block(Pos(2,2),Pos(3,2)),List(Down, Right)),
    (Block(Pos(1,1),Pos(1,1)),List(Down, Up)))

List(
    (Block(Pos(1,1),Pos(1,1)),List()),
    (Block(Pos(1,2),Pos(1,3)),List(Right)),
    (Block(Pos(2,1),Pos(3,1)),List(Down)),
    (Block(Pos(1,1),Pos(1,1)),List(Left, Right)),
    (Block(Pos(1,4),Pos(1,4)),List(Right, Right)),
    (Block(Pos(2,2),Pos(2,3)),List(Down, Right)),
    (Block(Pos(2,2),Pos(3,2)),List(Right, Down)),
    (Block(Pos(1,1),Pos(1,1)),List(Up, Down)),
    (Block(Pos(2,4),Pos(3,4)),List(Down, Right, Right)),
    (Block(Pos(2,1),Pos(2,1)),List(Left, Down, Right)))
*/
