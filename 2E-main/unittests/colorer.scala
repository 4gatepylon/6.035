import org.scalatest.FunSuite

import edu.mit.compilers.codegen._
import edu.mit.compilers.parser._
import edu.mit.compilers.semantics._

object Reg2Webs {
  def mk(webs: Set[Web]): Map[Register, Set[Web]] = {
    webs.foldLeft(Map[Register, Set[Web]]())((reg2webs, web) => {
      reg2webs.contains(web.reg) match {
        case true => {
          val prevEntry: Set[Web] = reg2webs(web.reg)
          val withoutPrevEntry: Map[Register, Set[Web]] = reg2webs - web.reg
          withoutPrevEntry + (web.reg -> (prevEntry + web))
        }
        case false => {
          reg2webs + (web.reg -> Set(web))
        }
      }
    })
  }

  def hasAll(g: Graph[Web], webs: Set[Web]): Boolean =
    webs.foldLeft(true)((acc, web) => g.hasNode(web))

  def numEdges(g: Graph[Web], webs1: Set[Web], webs2: Set[Web]): Int = {
    assert(webs1 != webs2)
    val (websList1, websList2) = (webs1.toList, webs2.toList)
    websList1.map(web1 => websList2.filter(g.hasEdge(web1, _)).size).sum
  }
}

class ColorerTester extends FunSuite {

  def getCFG(decaf: String): BasicBlock = {
    // Scanner has debug set to false because we design the decaf string
    val scanner: Scanner = new Scanner(decaf, false)
    val parser: Parser = new Parser(scanner)
    val root: ASTNode = parser.parse()
    assert(!parser.hasError)

    Pemdas.order(root)
    TableInserter.visit(root, Symbols(root))
    Checker.visit(root)
    assert(Collector.visit(root).isEmpty)

    val manager: CFGManager = new CFGManager(root)
    manager.mkCFGs()
    manager.cfgs("main")
  }

  test("Generic Graph Coloring Works") {
    val g = new Graph[Int]()
    for (i <- 0 until 5) {
      g.addNode(i)
    }
    g.addEdge(0, 1)
    g.addEdge(0, 2)
    g.addEdge(0, 3)
    g.addEdge(1, 2)
    g.addEdge(1, 3)
    g.addEdge(2, 4)
    g.addEdge(3, 4)

    val order: List[Int] = List(0, 3, 1, 2, 4)

    val expectedColoring: Map[Int, Int] =
      Map.apply(0 -> 0, 3 -> 1, 1 -> 2, 2 -> 1, 4 -> 0)
    val actualColoring =
      new GraphColorer[Int]((node: Int, i: Int) => None).color(g, order, 3)
    assert(actualColoring == expectedColoring)
  }

  test("Generic Graph Coloring Works (optimal order, crown graph)") {
    val g = new Graph[Int]()
    for (i <- 1 to 8) {
      g.addNode(i)
    }
    g.addEdge(1, 6)
    g.addEdge(1, 7)
    g.addEdge(1, 8)
    g.addEdge(2, 5)
    g.addEdge(2, 7)
    g.addEdge(2, 8)
    g.addEdge(3, 5)
    g.addEdge(3, 6)
    g.addEdge(3, 8)
    g.addEdge(4, 5)
    g.addEdge(4, 6)
    g.addEdge(4, 7)

    val order: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)

    val expectedColoring: Map[Int, Int] =
      Map.apply(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 1, 6 -> 1, 7 -> 1, 8 -> 1)
    val actualColoring =
      new GraphColorer[Int]((node: Int, i: Int) => None).color(g, order, 3)
    assert(actualColoring == expectedColoring)
  }

  test("Generic Graph Coloring Works (worst order, crown graph)") {
    val g = new Graph[Int]()
    for (i <- 1 to 8) {
      g.addNode(i)
    }
    g.addEdge(1, 4)
    g.addEdge(1, 6)
    g.addEdge(1, 8)
    g.addEdge(3, 2)
    g.addEdge(3, 6)
    g.addEdge(3, 8)
    g.addEdge(5, 2)
    g.addEdge(5, 4)
    g.addEdge(5, 8)
    g.addEdge(7, 2)
    g.addEdge(7, 4)
    g.addEdge(7, 6)

    val order: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)

    val expectedColoring: Map[Int, Int] =
      Map.apply(1 -> 0, 2 -> 0, 3 -> 1, 4 -> 1, 5 -> 2, 6 -> 2, 7 -> 3, 8 -> 3)
    val actualColoring =
      new GraphColorer[Int]((node: Int, i: Int) => None).color(g, order, 3)
    assert(actualColoring == expectedColoring)
  }

  test("Graph pruning: all are pruned") {
    val decaf: String = """
    int func(int x) {
      return x;
     }

     void main() {
     int a, b;
     a = 0;
     b = 0;
     func(a);
     func(b);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(aWebs.size == 1)
    assert(bWebs.size == 1)
    val aWeb = aWebs.head
    val bWeb = bWebs.head

    val graph = WebGrapher.makeGraph(webs)

    val expectedWebs: List[Web] = List(bWeb, aWeb)

    val actualWebs = WebColorer.pruneGraph(graph, 3)
    assert(actualWebs == expectedWebs)
  }

  test("Graph pruning: not all are pruned") {
    val decaf: String = """
    int func(int x) {
      return x;
     }

     void main() {
      int a, b, c, d, e;
      // a interferes with b, c, d, e
      // b interferes with a, e
      // c interferes with a, d, e
      // d interferes with a, c, e
      // e interferes with a, b, c, d
      a = 0;
      e = 0;

      c = 0;
      d = 0;
      func(c);
      func(d);

      b = 0;
      func(a);
      func(b);
      func(e);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    val cWebs = webs.filter(_.reg == rT.get("c")).toList
    val dWebs = webs.filter(_.reg == rT.get("d")).toList
    val eWebs = webs.filter(_.reg == rT.get("e")).toList
    assert(aWebs.size == 1)
    assert(bWebs.size == 1)
    assert(cWebs.size == 1)
    assert(dWebs.size == 1)
    assert(eWebs.size == 1)
    val aWeb = aWebs.head
    val bWeb = bWebs.head
    val cWeb = cWebs.head
    val dWeb = dWebs.head
    val eWeb = eWebs.head

    val graph = WebGrapher.makeGraph(webs)

    val expectedWebs: List[Web] = List(bWeb)

    val actualWebs = WebColorer.pruneGraph(graph, 3)
    assert(actualWebs == expectedWebs)
  }

  test("Creating graph from webs works: No interference") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b;
      a = 0;
      func(a);
      b = 0;
      func(b);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(aWebs.size == 1)
    assert(bWebs.size == 1)
    val aWeb = aWebs.head
    val bWeb = bWebs.head

    val graph = WebGrapher.makeGraph(webs)

    assert(graph.hasNode(aWeb))
    assert(graph.hasNode(bWeb))
    assert(!graph.hasEdge(aWeb, bWeb))
  }

  test("Creating graph from webs works: Interference in block") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b;
      a = 0;
      b = 0;
      func(a);
      func(b);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(aWebs.size == 1)
    assert(bWebs.size == 1)
    val aWeb = aWebs.head
    val bWeb = bWebs.head

    val graph = WebGrapher.makeGraph(webs)

    assert(graph.hasNode(aWeb))
    assert(graph.hasNode(bWeb))
    assert(graph.hasEdge(aWeb, bWeb))
  }

  test("Creating graph from webs works: Interference in block #2") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b, c;
      // a interferes with b
      a = 0;
      b = 0;
      func(a);
      func(b);

      // b interferes with c
      c = 0;
      func(b);
      func(c);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val regToWebs: Map[Register, Set[Web]] = Reg2Webs.mk(webs)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = regToWebs(rT.get("a"))
    val bWebs = regToWebs(rT.get("b"))
    val cWebs = regToWebs(rT.get("c"))

    val graph = WebGrapher.makeGraph(webs)
    assert(aWebs.size >= 1, "should have at least 1 webs for a")
    assert(bWebs.size >= 1, "should have at least 1 webs for b")
    assert(cWebs.size >= 1, "should have at least 1 webs for c")

    assert(Reg2Webs.hasAll(graph, aWebs ++ bWebs ++ cWebs))
    // Should be the case that `a` intersects `b` for its first
    // web, and `b` intersects `c` for `b's` second web. Note that
    // there may be more def-uses and/or webs due to temporaries.
    assert(
      Reg2Webs.numEdges(graph, aWebs, bWebs) == 1,
      "a and b should intersect"
    )
    assert(
      Reg2Webs.numEdges(graph, aWebs, cWebs) == 0,
      "a and c shuold not intersect"
    )
    assert(
      Reg2Webs.numEdges(graph, bWebs, cWebs) == 1,
      "b and c should intersect"
    )
  }

  test("Getting web spill cost: No loop") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a;
      a = 0;
      func(a);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))

    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    assert(aWebs.size == 1)
    val aWeb = aWebs.head

    val expectedSpillCost = 1
    val actualSpillCost = WebColorer.getWebSpillCost(aWeb)
    assert(expectedSpillCost == actualSpillCost)
  }

  test("Getting web spill cost: Loop") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a;
      a = 0;
      while(a < 1) {
        func(a);
      }
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    assert(aWebs.size == 1)
    val aWeb = aWebs.head

    val expectedSpillCost = 10
    val actualSpillCost = WebColorer.getWebSpillCost(aWeb)
    assert(expectedSpillCost == actualSpillCost)
  }

  test("Spilling webs: No webs spilled") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a;
      a = 0;
      func(a);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val graph = WebGrapher.makeGraph(webs)

    assert(WebColorer.spillGraph(graph, 3) == Set())
  }

  test("Spilling webs: 1 web spilled") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b, c;
      // a interferes with b
      a = 0;
      b = 0;
      func(a);
      func(b);

      // b interferes with c
      c = 0;
      func(b);
      func(c);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(bWebs.size == 1)
    val bWeb = bWebs.head

    val graph = WebGrapher.makeGraph(webs)

    assert(WebColorer.spillGraph(graph, 2) == Set(bWeb))
  }

  test("Graph Coloring with Webs works") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b, c;
      // a interferes with b
      a = 0;
      b = 0;
      func(a);
      func(b);

      // b interferes with c
      c = 0;
      func(b);
      func(c);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    // ...
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    assert(aWebs.size == 1)
    val aWeb = aWebs.head

    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(bWebs.size == 1)
    val bWeb = bWebs.head

    val cWebs = webs.filter(_.reg == rT.get("c")).toList
    assert(cWebs.size == 1)
    val cWeb = cWebs.head

    val graph = WebGrapher.makeGraph(webs)

    val expectedColors = Map(aWeb -> 1, bWeb -> 0, cWeb -> 1)
    val actualColors = WebColorer.colorWebs(graph, 2)

    assert(actualColors == expectedColors)
  }

  test("Graph Coloring with Webs works: no-spill example (lec 11, slide 48)") {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b, c, d, e;
      // a interferes with c, d
      // b interferes with e
      // c interferes with a, d, e
      // d interferes with a, c, e
      // e interferes with b, c, d
      a = 0;
      b = 0;
      func(b);
      c = 0;
      func(c);
      d = 0;
      func(d);
      func(a);

      // e interferes with c, d
      e = 0;
      func(c);
      func(d);
      func(e);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    // ...
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    assert(aWebs.size == 1)
    val aWeb = aWebs.head

    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(bWebs.size == 1)
    val bWeb = bWebs.head

    val cWebs = webs.filter(_.reg == rT.get("c")).toList
    assert(cWebs.size == 1)
    val cWeb = cWebs.head

    val dWebs = webs.filter(_.reg == rT.get("d")).toList
    assert(dWebs.size == 1)
    val dWeb = dWebs.head

    val eWebs = webs.filter(_.reg == rT.get("e")).toList
    assert(eWebs.size == 1)
    val eWeb = eWebs.head
    // ...

    val graph = WebGrapher.makeGraph(webs)
    val colors = WebColorer.colorWebs(graph, 3)
    assert(graph.isValidColoring(colors))
  }

  test(
    "Graph Coloring with Webs works: spill Example (lec 11, slide 64)"
  ) {
    val decaf: String = """
    int func(int x) {
      return x;
    }

    void main() {
      int a, b, c, d, e;
      // a interferes with b, c, d, e
      // b interferes with a, e
      // c interferes with a, d, e
      // d interferes with a, c, e
      // e interferes with a, b, c, d
      a = 0;
      e = 0;

      c = 0;
      d = 0;
      func(c);
      func(d);

      b = 0;
      func(a);
      func(b);
      func(e);
    }
    """
    val cfg = getCFG(decaf)
    val chains: Set[Chain] = Chainer.mk(cfg)
    val webs: Set[Web] = Webber.mergeChains(chains)

    val rT =
      cfg.regTable.getOrElse(throw new Exception("need regTable for test"))
    // ...
    val aWebs = webs.filter(_.reg == rT.get("a")).toList
    assert(aWebs.size == 1)
    val aWeb = aWebs.head

    val bWebs = webs.filter(_.reg == rT.get("b")).toList
    assert(bWebs.size == 1)
    val bWeb = bWebs.head

    val cWebs = webs.filter(_.reg == rT.get("c")).toList
    assert(cWebs.size == 1)
    val cWeb = cWebs.head

    val dWebs = webs.filter(_.reg == rT.get("d")).toList
    assert(dWebs.size == 1)
    val dWeb = dWebs.head

    val eWebs = webs.filter(_.reg == rT.get("e")).toList
    assert(eWebs.size == 1)
    val eWeb = eWebs.head
    // ...

    val graph = WebGrapher.makeGraph(webs)
    val colors = WebColorer.colorWebs(graph, 3)
    // valid coloring since only one spill
    assert(colors.values.filter(_ < 0).size == 1)
    assert(graph.isValidColoring(colors))
  }
}
