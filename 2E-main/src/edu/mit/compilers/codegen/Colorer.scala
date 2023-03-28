package edu.mit.compilers.codegen

// We use the mutable map because it makes graph code easier to understand
import edu.mit.compilers.optimization.LoopDepth

case class Graph[T](var nodes: Map[T, Set[T]] = Map[T, Set[T]]()) {
  // Data structure for dynamic undirected graphs
  // of unique elements (note how it is a Map)
  // NOTE that edges are two-way (and will appear twice:
  // once for each side)

  override def toString: String = {
    val sb = new StringBuilder
    for ((k, v) <- nodes) {
      sb.append(k + ":\n\t" + v.mkString("\n").replace("\n", "\n\t") + "\n\n")
    }
    sb.toString
  }

  // cannot use case class's .copy() because it is not a deep copy
  // override def clone: Graph[T] = new Graph[T](nodes.clone)

  def addNode(node: T): Unit = {
    assert(
      !nodes.contains(node),
      s"Tried to add node $node to graph, but it already exists"
    )
    nodes += (node -> Set())
  }

  def removeNode(node: T): Unit = {
    assert(
      nodes.contains(node),
      s"Tried to remove node $node from graph, but it does not exist"
    )
    nodes(node).foreach(n => removeEdge(n, node))
    nodes -= node
  }

  def addEdge(from: T, to: T): Unit = {
    assert(nodes.contains(from), s"Tried to add edge from missing node $from")
    assert(
      nodes.contains(to),
      s"Tried to add edge to missing node $to in graph $this"
    )
    assert(!nodes(from).contains(to))
    assert(!nodes(to).contains(from))
    // Graphs are 2-way
    nodes += (from -> (nodes(from) + to))
    nodes += (to -> (nodes(to) + from))
  }

  def removeEdge(from: T, to: T): Unit = {
    assert(nodes.contains(from), s"Tried to add edge from missing node $from")
    assert(nodes.contains(to), s"Tried to add edge to missing node $to")
    assert(nodes(from).contains(to))
    assert(nodes(to).contains(from))
    // Graphs are 2-way
    nodes += (from -> (nodes(from) - to))
    nodes += (to -> (nodes(to) - from))
  }

  def hasEdge(from: T, to: T): Boolean = {
    assert(nodes.contains(from), s"Tried to add edge from missing node $from")
    assert(nodes.contains(to), s"Tried to add edge to missing node $to")
    val fromTo = nodes(from).contains(to)
    val toFrom = nodes(to).contains(from)
    assert(fromTo == toFrom)
    fromTo
  }

  def hasNode(node: T): Boolean = nodes.contains(node)

  // Degree is useful for graph coloring
  def degree(node: T): Int = {
    assert(
      nodes.contains(node),
      s"Tried to get degree of $node, but it is not in graph"
    )
    nodes(node).size
  }

  def allNodes(): Set[T] = nodes.keySet.toSet

  def neighbors(node: T): Set[T] = {
    assert(
      nodes.contains(node),
      s"Tried to get neighbors of $node, but it is not in graph"
    )
    nodes(node).toSet
  }

  def isValidColoring(colors: Map[T, Int]): Boolean = {
    // Check that each node has a color
    if (colors.keySet.toSet != allNodes) return false
    // Check that each node's neighbors are different colors
    allNodes.forall(node => neighbors(node).forall(colors(node) != colors(_)))
  }
}

// Color a graph of node-type T with color-types C
// (in normal usage this will turn into coloring of type)
class GraphColorer[T](precolor: (T, Int) => Option[Int]) {
  def firstAvailColor(usedColors: Set[Int]): Int = {
    var count = 0
    while (usedColors.contains(count)) {
      count += 1
    }
    count
  }

  def color(graph: Graph[T], order: List[T], numRegisters: Int): Map[T, Int] = {
    var color: Map[T, Int] = Map()
    for (node <- order) {
      val preColor = precolor(node, numRegisters)
      if (preColor.isDefined) {
        color += (node -> preColor.get)
      } else {
        val neighbors = graph.nodes.getOrElse(node, Set[T]())
        val usedColors: Set[Int] =
          neighbors.map(n => color.getOrElse(n, -1)).filter(c => c != -1)
        color += (node -> firstAvailColor(usedColors))
      }
    }
    color
  }
}

object WebGrapher {
  // Turn webs into graphs
  def makeGraph(webs: Set[Web]): Graph[Web] = {
    val graph = new Graph[Web]()
    val websList = webs.toList
    websList.foreach(graph.addNode(_))

    // iterate over distinct pairs of webs. if they intersect, add that edge
    for (i <- 0 until websList.size - 1) {
      val web1 = websList(i)
      for (j <- i + 1 until websList.size) {
        val web2 = websList(j)
        if (web1.intersects(web2)) {
          graph.addEdge(web1, web2)
        }
      }
    }
    graph
  }
}

object WebColorer {
  // Color a web with RegLocs and Locs

  def getWebSpillCost(web: Web): Int = {
    val defBlocks: Set[BasicBlock] = web.defs.map(_.block).toSet
    val useBlocks: Set[BasicBlock] = web.uses.map(_.block).toSet
    val webBlocks: Set[BasicBlock] = defBlocks ++ useBlocks
    webBlocks.map(LoopDepth.getSpillCost).max
  }

  // Prunes graph of nodes with degree < numColors, returns a stack of pruned nodes w/ neighbors
  def pruneGraph(webGraph: Graph[Web], numColors: Int): List[Web] = {
    var prunedList: List[Web] = List()
    var changed = true
    while (changed) {
      val prunedNodes = webGraph.allNodes.filter(webGraph.degree(_) < numColors)
      if (prunedNodes.isEmpty) {
        changed = false
      } else {
        prunedNodes.foreach(webGraph.removeNode(_))
        prunedList ++= prunedNodes
        changed = true
      }
    }
    // reverse list to stack so graph can be rebuilt later
    prunedList.reverse
  }

  // Removes nodes with deg >= numColors, returns list of removed nodes
  def spillGraph(webGraph: Graph[Web], numColors: Int): Set[Web] = {
    var spilledNodes: Set[Web] = Set()
    var changed = true
    while (changed) {
      // nodes with degree >= numColors
      val largeNodes = webGraph.allNodes.filter(webGraph.degree(_) >= numColors)
      if (largeNodes.nonEmpty) {
        // get node with minimal spill cost
        val spilledNode = largeNodes
          .map(node => (node, getWebSpillCost(node)))
          .minBy(_._2)
          ._1
        spilledNodes += spilledNode
        webGraph.removeNode(spilledNode)
        changed = true
      } else {
        changed = false
      }
    }
    spilledNodes
  }

  // Colors webs based on slides in lec11; color of -1 = no color, must be Loc
  def colorWebs(webGraph: Graph[Web], registerCount: Int): Map[Web, Int] = {
    // Prune and spill nodes until graph empty
    val copiedGraph: Graph[Web] = webGraph.copy()
    var webStack: List[Web] = List()
    var spilledNodes: Set[Web] = Set()
    while (!copiedGraph.allNodes.isEmpty) {
      webStack = pruneGraph(copiedGraph, registerCount) ++ webStack
      spilledNodes ++= spillGraph(copiedGraph, registerCount)
    }

    // Precolor for globals to be allocated in Data
    def precolor(node: Web, i: Int): Option[Int] = {
      node.reg match {
        case reg: Addr => {
          if (
            reg.location == AddrLocation.Data ||
            reg.isInstanceOf[ArrElemAddr] ||
            reg.isInstanceOf[ArrBaseAddr]
          )
            Some(-1)
          else
            None
        }
        case _ =>
          None
      }
    }
    // Color new graph
    val colors: Map[Web, Int] =
      new GraphColorer[Web](precolor).color(webGraph, webStack, registerCount)
    // Add spilled nodes to colors
    spilledNodes.foldLeft(colors)((c, n) => c + (n -> -1))
  }
}
