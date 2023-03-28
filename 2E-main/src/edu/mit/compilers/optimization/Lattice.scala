package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._

// Now that Latice is a WIP file that is not actually necessary to any
// functionality, but will potentially give us a booste later on if we want to
// do some pretty hardcore dataflow optimizations. The idea is to create a
// program that can generally do the fixed point algorithm from class given any
// type of information you want to propagate throughout the program and act on.
//
// Things that remain are:
//   1. Implementing DCE using this lattice
//   2. Implementing CP and CSE using this lattice
//   3. Figuring out how to deal with the fact that sometimes you may need to run
//      each of these optimizations multiple times in sequence if you modify the CFG
//      only once right after the fixed point algorithm (for the propagation of
//      state information, like available expressions, instead of during its
//      calculation).
//   4. Figuring out how to generalize the abstraction function in some way so that
//      all our dataflow optimizations can override it.
//   5. See how we might want to enable loop hoisting (etc).
//
// It would be nice to have lattices act like this but it's not worth the effort
// https://stackoverflow.com/questions/663254/why-doesnt-the-example-compile-aka-how-does-co-contra-and-in-variance-w/674090#674090

// TODO move this somewhere else
case object LatticeUtils {
  def zipper[K, V](map1: Map[K, V], map2: Map[K, V], default: V) = {
    for (key <- map1.keys ++ map2.keys)
      yield (key, map1.getOrElse(key, default), map2.getOrElse(key, default))
  }
}

// Lattice Rules to handle elements of type T
trait Lattice[T] {
  def bottom: T
  def top: T
  def lub(x: T, y: T): T
  def glb(x: T, y: T): T

  // For example, the true lattice and the false lattice are duals
  def dual(): Lattice[T] = new Lattice[T] {
    def bottom = Lattice.this.top
    def top = Lattice.this.bottom
    def lub(x: T, y: T) = Lattice.this.glb(x, y)
    def glb(x: T, y: T) = Lattice.this.lub(x, y)
  }
}

// Abstraction function that maps (i.e. program states) to abstract states
// trait Abstraction[S] {
//   def af(instr: BasicInstr): S
// }

// S is the type of the state
// T is the type of the data
// O is the type of the Lattice (i.e. specifies rules)
// trait VectorLattice[S, T, O <: Lattice[T]] extends Lattice[Map[S, T]] {
//   val vector: Map[S, T] = Map()
//   def bottom: Map[S, T] = Map()
//   def lub(y: Map[S, T]: Map[S, T <: Lattice] =
//     zipper(vector, y, bottom).map(kvy => (kvy._1, kvy._2.lub(kvy._3))).toMap
//   def tf(instr: BasicInstr): Map[S, T <: Lattice] =
//     vector.map(kv => (kv._1, kv._2.tf(instr))).toMap
//    def tf(x: T, instr: BasicInstr): T
// }

case object BooleanLattices {
  // Would be nice to stop people from using _ProtoLattice...
  case object _ProtoLattice extends Lattice[Boolean] {
    def bottom = false
    def top = true
    def lub(x: Boolean, y: Boolean) = x || y
    def glb(x: Boolean, y: Boolean) = x && y
  }
  val TrueLattice = _ProtoLattice
  val FalseLattice = _ProtoLattice.dual()
}

// object LatticeOperator[T] {
//     // Return the outgoing values of every cfg node
//   def fixedPoint(cfg: BasicBlock): Map[BasicBlock, List[Set[Register]]] = {
//     var worklist: List[BasicBlock] = cfg.visit(basicBlock => basicBlock)

//     // An outgoing map that tells you the outgoing lattice value previously
//     var outgoings: Map[BasicBlock, List[Set[Register]]] =
//       worklist.map(bb => (bb, List.fill(bb.instrs.size + 1)(bottom))).toMap

//     // A "map" function that tells you the incoming lattice value
//     def incoming(bb: BasicBlock): Set[Register] = {
//       // Remember that the outgoings are in the same order as as the instructions (interleaved)
//       val children = bb.children
//       // println(
//       //   s"********************** children of ${bb.id}: ${children.map(_.id)}***************"
//       // )
//       lubIncoming(bb, outgoings)
//     }

//     // While things change, make sure to continue
//     // (if across a basic block nothing changes, then
//     // within it it can't either, so we only need to
//     // check blocks like in lecture)
//     while (worklist.size > 0) {
//       // println(
//       //   s"*********************** worklist starts as ${worklist.map(_.id)} ***********************"
//       // )
//       val n = worklist.head
//       worklist = worklist.drop(1)
//       // println(
//       //   s"************************* head ${n.id} outgoings \n${outgoings(n).mkString(",")}\n *************************"
//       // )

//       val prev_outgoings: List[Set[Register]] = outgoings(n)
//       // println(s"**** ${n.id}")
//       val new_incoming: Set[Register] = incoming(n)
//       // println(
//       //   s"************************* node ${n.id} new_incoming: ${new_incoming} *************************"
//       // )
//       val new_outgoings: List[Set[Register]] = tfBlock(new_incoming, n)
//       if (prev_outgoings != new_outgoings) {
//         outgoings = outgoings - n
//         outgoings = outgoings + (n -> new_outgoings)
//         // println(
//         //   s"********************* adding for node ${n.id} outgoing ${new_outgoings.head} *********************"
//         // )
//         // NOTE: this moves backwards
//         worklist = worklist ++ n.parents
//       }
//     }
//     outgoings
//   }
// }
