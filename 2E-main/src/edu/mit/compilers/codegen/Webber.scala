package edu.mit.compilers.codegen

import scala.collection.mutable.{Map => MutableMap}

class Merger[T](val mergeable: (T, T) => Boolean) {
  // Scala implementation of union find that does union find if
  // any of the elements in one set are mergeable with any of the
  // elements in the other set. NOT optimized. It is hard for me
  // to see how to make fast union find with the unfortunate
  // property that ANY element being mergeable with ANY other element
  // signifies that ALL the elements should be merged.

  def canMerge(a: Set[T], b: Set[T]): Boolean = {
    for (x <- a) {
      for (y <- b) {
        if (mergeable(x, y)) {
          return true
        }
      }
    }
    false
  }

  def groupInto(set: Set[T], sets: Set[Set[T]]): Set[Set[T]] = {
    // maximally merge set into sets until set is not mergeable with any
    assert(!sets.contains(set), "set must not be in sets")
    // find mergeable sets
    val mergeable = sets.filter(canMerge(set, _))
    if (mergeable.isEmpty) {
      sets + set
    } else {
      // created merged set
      val merged = mergeable.foldLeft(set)(_ ++ _)
      // recurse on merged set with remaining sets
      groupInto(merged, sets -- mergeable)
    }
  }

  def groupLoop(sets: Set[Set[T]]): Set[Set[T]] = {
    // merge the first mergeable set into the rest
    for (set <- sets) {
      val otherSets = sets - set
      if (otherSets.exists(canMerge(set, _))) {
        return groupInto(set, otherSets)
      }
    }
    sets
  }

  def group(collection: Set[T]): Set[Set[T]] = {
    var sets = collection.map(x => Set(x)).toSet
    var changed = true
    // Fixed Point Algorithm (SLOW, but should work in <= n iterations
    // if there are n elements). Merges pairs continually.
    while (changed) {
      val newSets = groupLoop(sets)
      changed = newSets != sets
      sets = newSets
    }
    sets
  }
}

case class Web(val chains: Set[Chain]) {
  override def toString(): String =
    s"Web(\n${defs.mkString("\n")}\n->\n${uses.mkString("\n")}\n)"

  def defs: Set[Def] = chains.map(_.definition).toSet
  def uses: Set[Use] = chains.map(_.usage).toSet
  def reg: Register = {
    val regs = chains.map(_.reg)
    assert(regs.size == 1)
    regs.head
  }
  def contains(that: Web): Boolean = {
    assert(this != that)
    val thisDefs: Set[Def] = defs
    val thisUses: Set[Use] = uses
    val thatDefs: Set[Def] = that.defs
    val thatUses: Set[Use] = that.uses
    // NOTE a def and use can be on the same line and share a register
    for (d <- thisDefs) {
      for (u <- thisUses) {
        // Defs
        for (_d <- thatDefs) {
          // I don't like this
          if (d <= _d && (_d < u)) {
            return true
          }
        }
        // Uses
        for (_u <- thatUses) {
          // I don't like this
          if ((d < _u) && _u <= u) {
            return true
          }
        }
      }
    }
    false
  }
  def intersects(that: Web): Boolean =
    this.contains(that) || that.contains(this)
}

object Webber {
  // Object to create webs for a single CFG given def-use chains
  // (note that each def-use forms a smaller chain and those chains
  // are merged together to form the largest possible chain)

  def mergeableChains(chain1: Chain, chain2: Chain): Boolean =
    chain1.definition == chain2.definition || chain1.usage == chain2.usage

  def mergeChains(chains: Set[Chain]): Set[Web] = {
    // Sanity test our previous implementation
    assert(chains.forall(_.isValid), "Some chain is invalid before merging")

    val merger: Merger[Chain] = new Merger(mergeableChains)
    val groups: Set[Set[Chain]] = merger.group(chains)

    // There should be no empty groups and the groups should have
    // all the elements inside of them.
    val groupsSize = groups.toList.map(_.size).sum
    assert(
      groupsSize == chains.size,
      "Groups (set of chains) did not contain all chains"
    )

    groups.map(Web(_)).toSet
  }
}

object WebFinder {
  def defOrUseToWeb(webs: Set[Web]): Map[DefOrUse, Web] =
    webs.flatMap(web => (web.defs ++ web.uses).map(du => (du, web))).toMap
}
