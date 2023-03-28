import org.scalatest.FunSuite

// Make a graph
import edu.mit.compilers.codegen.Graph

class GraphTestSuite extends FunSuite {
  test("(Generic) Can insert and remove nodes") {
    var g: Graph[Int] = new Graph()

    val x1 = 1
    val x2 = 2
    assert(!g.hasNode(x1))
    assert(!g.hasNode(x2))

    g.addNode(x1)
    assert(g.hasNode(x1))

    g.addNode(x2)
    assert(g.hasNode(x1))
    assert(g.hasNode(x2))

    g.removeNode(x1)
    assert(!g.hasNode(x1))
    assert(g.hasNode(x2))

    g.removeNode(x2)
    assert(!g.hasNode(x1))
    assert(!g.hasNode(x2))
  }

  test("(Generic) Can Create and Remove Edges") {
    var g: Graph[Int] = new Graph()

    val x1 = 1
    val x2 = 2
    val x3 = 3
    g.addNode(x1)
    g.addNode(x2)
    g.addNode(x3)

    // Create a triangle
    g.addEdge(x1, x2)
    g.addEdge(x2, x3)
    g.addEdge(x3, x1)

    // Make sure the edges we added are there
    assert(g.hasEdge(x1, x2))
    assert(g.hasEdge(x2, x3))
    assert(g.hasEdge(x3, x1))

    // Make sure the reverse edges are present
    // (remember this is an undirected graph)
    assert(g.hasEdge(x1, x3))
    assert(g.hasEdge(x2, x1))
    assert(g.hasEdge(x3, x2))

    g.removeEdge(x1, x3)
    // Make sure that edge was removed
    assert(!g.hasEdge(x1, x3))
    assert(!g.hasEdge(x3, x1))
    // Make sure other things were not removed
    assert(g.hasEdge(x1, x2))
    assert(g.hasEdge(x2, x1))
    assert(g.hasEdge(x2, x3))
    assert(g.hasEdge(x3, x2))

    // Remove a node and make sure that all its edges are removed
    // (note that default behavior is to throw an error if either node
    // is not present)
    g.removeNode(x2)
    g.addNode(x2)
    assert(!g.hasEdge(x1, x2))
    assert(!g.hasEdge(x2, x1))
    assert(!g.hasEdge(x2, x3))
    assert(!g.hasEdge(x3, x2))
  }

  test("(Generic) Degrees are correct") {
    var g: Graph[Int] = new Graph()

    val x1 = 1
    val x2 = 2
    val x3 = 3
    g.addNode(x1)
    g.addNode(x2)
    g.addNode(x3)

    // Create a triangle
    g.addEdge(x1, x2)
    g.addEdge(x2, x3)
    g.addEdge(x3, x1)

    // Make sure the degrees are correct
    assert(g.degree(x1) == 2)
    assert(g.degree(x2) == 2)
    assert(g.degree(x3) == 2)

    // Remove a node and see how that changes the degree
    g.removeNode(x1)
    assert(g.degree(x2) == 1)
    assert(g.degree(x3) == 1)

    // Remove an edge and see how that changes the degree
    g.removeEdge(x2, x3)
    assert(g.degree(x2) == 0)
    assert(g.degree(x3) == 0)

    // Add an edge and see how that changes the degree
    g.addEdge(x2, x3)
    assert(g.degree(x2) == 1)
    assert(g.degree(x3) == 1)
  }
}
