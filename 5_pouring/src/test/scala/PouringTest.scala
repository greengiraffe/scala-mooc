import org.scalatest._
import pouring._

class PouringTest extends FlatSpec {

  def fixture = new {
    val problem = new Pouring(Vector(4,7))
  }

  "A (4,7) Pouring Problem" should "initially have empty glasses" in {
    assert(fixture.problem.initialState == Vector(0,0))
  }

  it should "generate a set of paths" in {
    assert(fixture.problem.pathSets.nonEmpty)
  }

  it should "generate at least one solution for target 6" in {
    val solutions = fixture.problem.solution(6)
    print(solutions.take(3).toList)
    print(solutions.length)
    assert(solutions.nonEmpty)
  }
}
