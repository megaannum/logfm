package com.megaannum.logging.util

import scala.collection.mutable.Stack
/*

parent
  leaf
  parent
    leaf
    leaf
  leaf
  leaf
  parent
    leaf
    parent
      leaf
  parent
      leaf

parent: 0 - N children
  append Leaf
  prepend Leaf
Leaf: E


*/
/*
    def append(e: E): Node[E] = append(LeafNode[E](e))
    def append(node: Node[E]): Node[E]
    def prepend(e: E): Node[E] = prepend(LeafNode[E](e))
    def prepend(node: Node[E]): Node[E]
    def appendChild(e: E): Node[E] = appendChild(LeafNode[E](e))
    def appendChild(node: Node[E]): Node[E]
*/
/*
    def join(e1: E, e2: E): List[E] = List(e1, e2)
    def concat(c1: List[E], c2: List[E]): List[E] = c1 ::: c2
    def append(e: E, c: List[E]): List[E] = e :: c
    def prepend(c: List[E], e: E): List[E] = c :+ e


HELPER OPS
    def join(e1: E, e2: E): C
    def append(e: E, c: C): C
    def concat(c1: C, c2: C): C
    def prepend(c: C, e: E): C
    def single(e: E): C
    def empty: C
    // Tree operations defaulting to List operations
    def appendChild(child: E, parent: C): C = append(child, parent)
    def prependChild(parent: C, child: E): C = prepend(parent, child) 
    def concatChildren(parent: C, children: C): C = concat(parent, children)

*/
/*
    def join(e1: E, e2: E): C
    def concat(c1: C, c2: C): C
    def append(e: E, c: C): C
    def prepend(c: C, e: E): C
    def single(e: E): C
    def empty: C
    // Tree operations defaulting to List operations
    def appendChild(child: E, parent: C): C = append(child, parent)
    def prependChild(parent: C, child: E): C = prepend(parent, child) 
    def concatChildren(parent: C, children: C): C = concat(parent, children)

    def take(c:C, n: Int): C
// DONE
    def drop(c:C, n: Int): C
    def slice(c:C, from: Int, until: Int): C
    def takeRigh(c:C, n: Int): C
    def splitAt(c:C, n: Int): (C, C)

    def isEmpty(c: C): Boolean
    def size(c: C): Int
    def exists(c: C, pn: E=>Boolean): Boolean
    def find(c: C, p: E=>Boolean): Option[E]
    def count(c: C, p: E=>Boolean): Int

    def mkString(c: C, start: String, sep: String, end: String ): String
    def mkString(c: C, sep: String): String = mkString(c, "", sep, "")
    def mkString(c: C): String = mkString(c, "")

    def reverse(c: C): C

    def foldLeft[V](c: C, v: V)(op: (V,E)=>V): V 
    def fold[V](c: C, v: V)(op: (V,E)=>V): V = foldLeft(c,v)(op)
    def foldRight[V](c: C, v: V)(op: (E,V)=>V): V = 
      foldLeft(reverse(c), v)((x,y) => op(y,x))

    def takeWhile(c: C)(fn: E=>Boolean): C
    def dropWhile(c: C)(fn: E=>Boolean): C
    def span(c: C)(fn: E=>Boolean): (C, C)
    def filter(c: List[E])(fn: E=>Boolean): C
    def sortWith(c: C)(lt: (E,E)=>Boolean): C

    def forall[U](c: C)(fn: E=>Boolean): Boolean
    def foreach[U](c: C)(fn: E=>Unit): Unit

    def map[F,D](c: C)(fn: E => F): D
*/
object Tree {

  trait Iter[E] {
    def hasNext: Boolean
    def next: Node[E]
  }

  // size of LeafNode's
  def toSize[E](n: Node[E]): Int = n match {
    case pn: ParentNode[E] =>
      pn.nodes.foldLeft(0) { (value,node) =>
        node match {
          case ParentNode(pn) => toSize[E](node) + value
          case ln: LeafNode[E] => value + 1
          case en: EmptyNode[E] => value
        }
      }
    case ln: LeafNode[E] => 1
    case en: EmptyNode[E] => 0
  }

  // list of LeafNode's
  def toDepthFirstList[E](n: Node[E]): List[Node[E]] = n match {
    case pn: ParentNode[E] =>
      pn.nodes.foldLeft(Nil: List[Node[E]]) { (list,n) =>
        n match {
          case ParentNode(pn) => toDepthFirstList[E](n) ::: list
          case ln: LeafNode[E] => ln :: list
          case en: EmptyNode[E] => list
        }
      }
    case ln: LeafNode[E] => List(ln)
    case en: EmptyNode[E] => Nil
  }
  // list of LeafNode's
  def toBreathFirstList[E](n: Node[E]): List[Node[E]] = n match {
    case pn: ParentNode[E] =>
      // current level
      pn.nodes.foldLeft(Nil: List[Node[E]]) { (list,n) =>
        n match {
          case ParentNode(pn) => Nil
          case ln: LeafNode[E] => ln :: list
          case en: EmptyNode[E] => list
        }
      }
      // go down a level
      pn.nodes.foldLeft(Nil: List[Node[E]]) { (list,n) =>
        n match {
          case ParentNode(pn) => toBreathFirstList[E](n) ::: list
          case ln: LeafNode[E] => Nil
          case en: EmptyNode[E] => Nil
        }
      }
    case ln: LeafNode[E] => List(ln)
    case en: EmptyNode[E] => Nil
  }

  class DepthFirstIter[E](pn: ParentNode[E]) extends Iter[E] {
    val iter = toDepthFirstList[E](pn).toIterator
    def hasNext: Boolean = iter.hasNext
    def next: Node[E] = iter.next
  }
  class BreathFirstIter[E](pn: ParentNode[E]) extends Iter[E] {
    val iter = toBreathFirstList[E](pn).toIterator
    def hasNext: Boolean = iter.hasNext
    def next: Node[E] = iter.next
  }

  class EmptyIter[E] extends Iter[E] {
    def hasNext: Boolean = false
    def next: Node[E] = throw new NoSuchElementException
  }
  class LeafIter[E](ln: LeafNode[E]) extends Iter[E] {
    var lnOp: Option[LeafNode[E]] = Some(ln)
    def hasNext: Boolean = lnOp.isDefined
    def next: Node[E] =  {
      if (lnOp.isDefined) {
        try { lnOp.get } finally { lnOp = None }
      } else throw new NoSuchElementException
    }
  }



  trait Monad[E] {
    def list(node: Node[E]): List[Node[E]]

    def empty[E]: Node[E] = EmptyNode[E]()
    def single[E](e: E): Node[E] = LeafNode[E](e)

    // e1,e2
    def join[E](e1: E, e2: E): Node[E] = ParentNode[E](List(LeafNode(e1),LeafNode(e2)))

    // c1 ::: c2
    def concat[E](node1: Node[E], node2: Node[E]): Node[E] = node1 match {
      case p1: ParentNode[E] => p1.concat(node2)
      case l1: LeafNode[E] => node2 match {
        case p2: ParentNode[E] => ParentNode[E](l1 :: p2.nodes)
        case l2: LeafNode[E] => ParentNode[E](List(node1,node2))
        case en: EmptyNode[E] => node1
      }
      case en: EmptyNode[E] => node2
    }

    // at end
    // e :: c
    def append[E](e: E, node: Node[E]): Node[E] = node match {
      case pn: ParentNode[E] => pn.prepend(LeafNode[E](e))
      case ln: LeafNode[E] => ParentNode[E](List(LeafNode[E](e),ln))
      case en: EmptyNode[E] => LeafNode[E](e)
    }

    // at beginning
    // c :+ e
    def prepend[E](node: Node[E], e: E): Node[E] = node match {
      case pn: ParentNode[E] => pn.append(LeafNode[E](e))
      case ln: LeafNode[E] => ParentNode[E](List(ln, LeafNode[E](e)))
      case en: EmptyNode[E] => LeafNode[E](e)
    }

    def isEmpty[E](node: Node[E]): Boolean = node match {
      case pn: ParentNode[E] => false
      case ln: LeafNode[E] => false
      case en: EmptyNode[E] => true
    }
    def size[E](node: Node[E]): Int = toSize(node)

    def take(node: Node[E], n: Int): List[Node[E]] = 
      list(node).take(n)
    def drop(node: Node[E], n: Int): List[Node[E]] =
      list(node).drop(n)
    def slice(node: Node[E], from: Int, until: Int): List[Node[E]] = 
      list(node).slice(from,until)
    def takeRight(node: Node[E], n: Int): List[Node[E]] =
      list(node).takeRight(n)
    def splitAt(node: Node[E], n: Int): (List[Node[E]], List[Node[E]]) =
      list(node).splitAt(n)


    def exists(node: Node[E], p: Node[E]=>Boolean): Boolean =
      list(node).exists(p)
    def find(node: Node[E], p: Node[E]=>Boolean): Option[Node[E]] =
      list(node).find(p)
    def count(node: Node[E], p: Node[E]=>Boolean): Int = 
      list(node).count(p)

    def mkString(node: Node[E], start: String, sep: String, end: String ): String = ???
    def mkString(node: Node[E], sep: String): String = mkString(node, "", sep, "")
    def mkString(node: Node[E]): String = mkString(node, "")

    def reverse(node: Node[E]): List[Node[E]] = list(node).reverse

    def foldLeft[V](node: Node[E], v: V)(op: (V,Node[E])=>V): V = 
      list(node).foldLeft(v)(op)
    def fold[V](node: Node[E], v: V)(op: (V,Node[E])=>V): V = 
      foldLeft(node,v)(op)
    def foldRight[V](node: Node[E], v: V)(op: (Node[E],V)=>V): V = 
      reverse(node).foldLeft(v)((x,y) => op(y,x))

    def takeWhile(node: Node[E])(fn: Node[E]=>Boolean): List[Node[E]] = 
      list(node).takeWhile(fn)
    def dropWhile(node: Node[E])(fn: Node[E]=>Boolean): List[Node[E]] =
      list(node).dropWhile(fn)
    def span(node: Node[E])(fn: Node[E]=>Boolean): (List[Node[E]], List[Node[E]]) =
      list(node).span(fn)

    def filter(node: Node[E])(fn: Node[E]=>Boolean): List[Node[E]] = 
      list(node).filter(fn)

    def sortWith(node: Node[E])(lt: (Node[E],Node[E])=>Boolean): List[Node[E]] =
      list(node).sortWith(lt)

    def forall(node: Node[E])(fn: Node[E]=>Boolean): Boolean = 
      list(node).forall(fn)

    def foreach[U](node: Node[E])(fn: Node[E]=>U): Unit = 
      list(node).foreach(fn)

    def map[F](node: Node[E])(fn: Node[E] => Node[F]): List[Node[F]] = 
      list(node).map(fn)

  }
  trait DepthFirstMonad[E] extends Monad[E] {
    def list(node: Node[E]): List[Node[E]] = 
      toDepthFirstList[E](node)
  }
  trait BreathFirstMonad[E] extends Monad[E] {
    def list(node: Node[E]): List[Node[E]] = 
      toBreathFirstList[E](node)
  }

/*
  object Monad {
    implicit def monad[E]: Monad[E] = new Monad[E] {}
    implicit def stringMonad: Monad[String] = new Monad[String] { }
  }
*/
  object DepthFirstMonad {
    implicit def monad[E]: Monad[E] = new DepthFirstMonad[E] {}
    implicit def stringMonad: Monad[String] = new DepthFirstMonad[String] { }
  }
  object BreathFirstMonad {
    implicit def monad[E]: Monad[E] = new BreathFirstMonad[E] {}
    implicit def stringMonad: Monad[String] = new BreathFirstMonad[String] { }
  }



  def empty[E]: Node[E] = EmptyNode[E]()
  
/*
  def appendChild[E](child: Node[E], parent: Node[C]): Node[E] = {
  }
    // Tree operations defaulting to List operations
    def appendChild(child: E, parent: C): C = append(child, parent)
    def prependChild(parent: C, child: E): C = prepend(parent, child) 
    def concatChildren(parent: C, children: C): C = concat(parent, children)
*/

/*
  // depth first
  // traversal order
  def take[E](node: Node[E], n: Int): Node[E] = node match {
    case pn: ParentNode[E] => pn.nodes.take(n) match {
      case Nil => empty
      case ns => ParentNode(ns)
    }
    case ln: LeafNode[E] => if (n==1) ln else empty
    case en: EmptyNode[E] => empty
  }

  def drop[E](node: Node[E], n: Int): Node[E] = ???
  def slice[E](node: Node[E], from: Int, until: Int): Node[E] = ???
  def takeRigh[E](node: Node[E], n: Int): Node[E] = ???
  def splitAt[E](node: Node[E], n: Int): (Node[E], Node[E]) = ???

  def isEmpty[E](node: Node[E]): Boolean = node match {
    case pn: ParentNode[E] => false
    case ln: LeafNode[E] => false
    case en: EmptyNode[E] => true
  }

  def size[E](node: Node[E]): Int = node match {
    case pn: ParentNode[E] => pn.nodes.size
    case ln: LeafNode[E] => 1
    case en: EmptyNode[E] => 0
  }
*/




  sealed trait Node[E] {
    // at end
    def append(e: E): Node[E] = append(LeafNode(e))
    // at beginning
    def prepend(e: E): Node[E] = prepend(LeafNode(e))

    def append(node: Node[E]): Node[E]
    def prepend(node: Node[E]): Node[E]
  }

  final case class EmptyNode[E]() extends Node[E] {
    def append(node: Node[E]): Node[E] = node
    def prepend(node: Node[E]): Node[E] = node
  }


  final case class LeafNode[E](e: E) extends Node[E] {
    def append(node: Node[E]): Node[E] = ParentNode[E](List(this,node))
    def prepend(node: Node[E]): Node[E] = ParentNode[E](List(node,this))
  }
  
  final case class ParentNode[E](val nodes: List[Node[E]] = Nil) extends Node[E] {
    def append(node: Node[E]): ParentNode[E] = ParentNode(nodes :+ node)
    def prepend(node: Node[E]): ParentNode[E] = ParentNode(node :: nodes)

    def concat(node: Node[E]): ParentNode[E] = node match {
      case pn: ParentNode[E] => ParentNode(nodes ::: pn.nodes)
      case ln: LeafNode[E] => ParentNode(nodes :+ ln)
      case en: EmptyNode[E] => this
    }
  }
}

