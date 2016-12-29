package com.megaannum.logging.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.reflect.runtime.universe._

import com.megaannum.logging.util.Tree._

class TreeSpec extends FlatSpec with Matchers {

  it should "build a Tree" in {
    // val m = Monad.stringMonad
    val m = DepthFirstMonad.stringMonad

    val empty = m.empty[String]
    val leaf1 = m.single("One")
    val leaf2 = m.single("Two")
    val parent1 = leaf1.append("Three")
    val parent2 = leaf2.prepend("Four")
    val parent3 = parent1.prepend(parent2)
    
    empty should be (EmptyNode[String])
    leaf1 should be (LeafNode("One"))
    parent1 should be (ParentNode(List(LeafNode("One"),LeafNode("Three"))))
    parent2 should be (ParentNode(List(LeafNode("Four"),LeafNode("Two"))))
    parent3 should be (ParentNode(List(ParentNode(List(LeafNode("Four"),LeafNode("Two"))),LeafNode("One"),LeafNode("Three"))))

    parent1.append("Joe") should be (ParentNode(List(LeafNode("One"),LeafNode("Three"),LeafNode("Joe"))))
    parent1.prepend("Joe") should be (ParentNode(List(LeafNode("Joe"),LeafNode("One"),LeafNode("Three"))))
    parent2.prepend("Joe") should be (ParentNode(List(LeafNode("Joe"),LeafNode("Four"),LeafNode("Two"))))

    m.join("One","Three") should be (ParentNode(List(LeafNode("One"),LeafNode("Three"))))
    m.concat(empty,empty) should be (m.empty)
    m.concat(leaf1,empty) should be (leaf1)
    m.concat(empty,leaf1) should be (leaf1)
    m.concat(leaf1,leaf2) should be (ParentNode(List(leaf1,leaf2)))
    m.concat(empty,parent1) should be (parent1)
    m.concat(parent1,empty) should be (parent1)
    m.concat(leaf1,parent1) should be (ParentNode(List(LeafNode("One"),LeafNode("One"),LeafNode("Three"))))
    m.concat(parent1, leaf1) should be (ParentNode(List(LeafNode("One"),LeafNode("Three"),LeafNode("One"))))

  }
  it should "determine size" in {
    // val m = Monad.stringMonad
    val m = DepthFirstMonad.stringMonad

    val empty = m.empty[String]
    val leaf1 = m.single("One")
    val leaf2 = m.single("Two")
    val parent1 = leaf1.append("Three")
    val parent2 = leaf1.prepend("Four")
    val parent3 = parent1.prepend(parent2)

    m.isEmpty(empty) should be (true)
    m.isEmpty(leaf1) should be (false)
    m.isEmpty(parent2) should be (false)

    m.size(empty) should be (0)
    m.size(leaf1) should be (1)
    m.size(parent1) should be (2)
    m.size(parent3) should be (4)
  }
}
