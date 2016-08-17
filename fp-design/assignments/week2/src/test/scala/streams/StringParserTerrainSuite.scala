package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {
    class Test() extends StringParserTerrain {
        val level = ""

    }

    test("test terrainFunction with 'S' position.") {
        new Test {
            val pos = Pos(0,0)
            val target = terrainFunction(Vector(Vector('S', 'T', 'o')))(pos)
            assert(target, "S not found")
        }
    }

}
