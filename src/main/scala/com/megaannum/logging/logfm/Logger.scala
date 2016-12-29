
package com.megaannum.logging.logfm

import com.megaannum.logging.util.Union
import com.megaannum.logging.util.Union._
import scala.util.{Try, Success, Failure}
import scala.util.{Either, Left, Right}
import scala.reflect.runtime.universe._



// http://blog.tmorris.net/posts/the-writer-monad-using-scala-example/


/**
 *  Option[T]: 
 *    Use when a value can be absent or some validation can fail and you don't 
 *    care about the exact cause. Typically in data retrieval and validation logic.
 *  Either[L,R]: 
 *    Similar use case as Option but when you do need to provide some information 
 *    about the error.
 *  Try[T]: 
 *    Use when something Exceptional can happen that you cannot handle in the 
 *    function. This, in general, excludes validation logic and data retrieval 
 *    failures but can be used to report unexpected failures.
 *  Exception:
 *    Use only as a last resort. When catching exceptions use the facility 
 *    methods Scala provides and never catch { _ => }, instead use 
 *    catch { NonFatal(_) => }
 *  From: http://blog.xebia.com/try-option-or-either/
 */
object Logger {

  sealed trait Node[+K]
  trait Error[+K] extends Node[K]
  final case object OptionError extends Error[Nothing]
  final case class EitherError[L](s: L) extends Error[Nothing]
  final case class TryError(ex: Throwable) extends Error[Nothing]
  final case class Value[+K](k: K) extends Node[K]

  /** Monadic operations supported by Logger Elements E and Logger Container C.
   *
   * Generally, it is expected that the Container will be a List or Tree 
   * and the Elements will be a String or some case class.
   *
   * @tparm E   Log Element type
   * @tparm C   Log Container type
   */
  trait Monad[E,C] {
    def as(x: Any): C = x.asInstanceOf[C]

    def join(e1: E, e2: E): C
    def concat(c1: C, c2: C): C
    def flatten(cs: List[C]): C
    def append(e: E, c: C): C
    def prepend(c: C, e: E): C
    def single(e: E): C
    def empty: C
    // Tree operations defaulting to List operations
    def appendChild(child: E, parent: C): C = append(child, parent)
    def prependChild(parent: C, child: E): C = prepend(parent, child) 
    def concatChildren(parent: C, children: C): C = concat(parent, children)

    def take(c:C, n: Int): C
    def drop(c:C, n: Int): C
    def slice(c:C, from: Int, until: Int): C
    def takeRigh(c:C, n: Int): C
    def splitAt(c:C, n: Int): (C, C)

    def isEmpty(c: C): Boolean
    def size(c: C): Int
    def exists(c: C, p: E=>Boolean): Boolean
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
/*
    def flatMap[F,D](c: C)(fn: E => Option[F]): D
*/
  }
  trait ListMonad[E] extends Monad[E, List[E]] { 
    def join(e1: E, e2: E): List[E] = List(e1, e2)
    def concat(c1: List[E], c2: List[E]): List[E] = c1 ::: c2
    def flatten(cs: List[List[E]]): List[E] = cs.flatten
    def append(e: E, c: List[E]): List[E] = e :: c
    def prepend(c: List[E], e: E): List[E] = c :+ e
    def single(e: E): List[E] = List(e)
    def empty: List[E] = Nil

    def take(c: List[E], n: Int): List[E] = as(c).take(n)
    def drop(c: List[E], n: Int): List[E] = as(c).drop(n)
    def slice(c: List[E], from: Int, until: Int): List[E] = as(c).slice(from, until)
    def takeRigh(c: List[E], n: Int): List[E] = as(c).takeRight(n)
    def splitAt(c: List[E], n: Int): (List[E], List[E]) = as(c).splitAt(n)

    def isEmpty(c: List[E]): Boolean = as(c).isEmpty
    def size(c: List[E]): Int = as(c).size
    def exists(c: List[E], p: E=>Boolean): Boolean = as(c).exists(p)
    def find(c: List[E], p: E=>Boolean): Option[E] = as(c).find(p)
    def count(c: List[E], p: E=>Boolean): Int = as(c).count(p)

    def mkString(c: List[E], start: String, sep: String, end: String ): String =
      as(c).mkString(start, sep, end)


    def reverse(c: List[E]): List[E] = as(c).reverse

    def foldLeft[V](c: List[E], v: V)(op: (V,E)=>V): V = c.foldLeft(v)(op)
      
    def takeWhile(c: List[E])(fn: E=>Boolean): List[E] = as(c).takeWhile(fn)
    def dropWhile(c: List[E])(fn: E=>Boolean): List[E] = as(c).dropWhile(fn)
    def span(c: List[E])(fn: E=>Boolean): (List[E], List[E]) = as(c).span(fn)
    def filter(c: List[E])(fn: E=>Boolean): List[E] = as(c).filter(fn)
    def sortWith(c: List[E])(lt: (E,E)=>Boolean): List[E] = as(c).sortWith(lt)

    def forall[U](c: List[E])(fn: E=>Boolean): Boolean = as(c).forall(fn)
    def foreach[U](c: List[E])(fn: E=>Unit): Unit = as(c).foreach(fn)

    def map[F,D](c: List[E])(fn: E => F): D = as(c).map(fn).asInstanceOf[D]

/*
    // Note: could not get this to compile
    def flatMap[F,D](c: List[E])(fn: E => Option[F]): D =
      as(c).flatMap(fn).asInstanceOf[D]
*/
  }

  object Monad {
    implicit def listMonad[E]: Monad[E, List[E]] = 
      new ListMonad[E] { }

    implicit def stringListMonad: Monad[String, List[String]] = 
      new ListMonad[String] { }
  }
  {
    val ms = Monad.listMonad[String]
    val mi = Monad.listMonad[Int]

    val ls: List[String] = List("hi", "bye")
    println(s" ls=$ls")
    val li: List[Int] = ms.map(ls) { s => s.length }
    println(s" li=$li")
    val i = mi.fold(li, 0)((v,e) => v+e)
    println(s" i=$i")
  }

  /** Special operations supported by a Log with Logger Elements E 
   * and Logger Container C.
   *
   * These are used to generate the "ENTER" and "LEAVE" log messages in
   * the enter_leave method below.
   *
   * If there is a better way of supporting this, I am open to suggestions.
   *
   * @tparm E   Log Element type
   * @tparm C   Log Container type
   */
  trait LogHelper[E,C] {
    def enter: E
    def leave: E
  }
  trait ListLogHelper[E] extends LogHelper[E, List[E]]{
    def enter: E
    def leave: E
  }
  object LogHelper {
    implicit def stringListLogHelper: LogHelper[String, List[String]] = 
      new ListLogHelper[String] {
        def enter: String = "ENTER"
        def leave: String = "LEAVE"
      }
  }

  type UnionTOE[L,W] = union [W] #or [Option[W]] #or [Either[L,W]] #or [Try[W]]
  type Wrapped[L,W] = Union[UnionTOE[L,W]]

  def unital[E,C,L: TypeTag,W: TypeTag](value: W)(implicit m: Monad[E,C]) = {
    val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
    wrapped.assign(value)
    Logger[E,C,L,W](m.empty, wrapped)
  }


  /*
   * Implicits:
   *    for method:
   *      ~> : if any error propagate left else right error
   *           alias 'with'
   *
   *  TODO:
   *    <~ errors from right to left
   *           alias 'with'
   *    Logger[W] concatLeft Logger[V] ignore left errors
   *    Logger[W] concatRight Logger[V] ignore right errors
   *    same with merge
   *
   *   ERROR|VAlUE>
   *
   *           Value  L    R    fn LR => L  fn LR => R
   *    Error
   *      L           L|L>  L|R>     L|LR=L>   L|LR=R>
   *      R           R|L>  R|R>     R|LR=>    R|LR=R>
   *      X
   *
   * 
   *  List(Logger[W]) ~> Logger[V]    == Logger[V]
   *  List(Logger[W]) flatten         == Logger[W]
   *  Logger[W] ~> E                  == Logger[W]
   *  Logger[W] op V WV=>V            == Logger[V]
   *  Logger[W] op Logger[V] WV=>W    == Logger[W]
   *  Logger[W] concat Logger[_]      == Logger[W]( +++ )
   *  Logger[W] merge List[Logger[_]] == Logger[W]( +++ )
   *  Logger[_] replace W             == Logger[W]
   *  Logger[_] replace Option[W]     == Logger[W]
   *  Logger[_] replace Either[W]     == Logger[W]
   *  Logger[_] replace Try[W]        == Logger[W]
   *  Logger[_] replace Logger[W]     == Logger[W]
   *  E ~> W                          == Logger[W]
   *  E ~> Option[W]                  == Logger[W]
   *  E ~> Either[W]                  == Logger[W]
   *  E ~> Try[W]                     == Logger[W]
   *  E ~> Logger                     == Logger[W]
   *  W <~ W=>E                       == Logger[W]
   */

  /////////////////////////////////////////////////////////////////////////////
  
  /** Implicit to automatically extract a Logger's value as an Option if it
   * exists.
   *
   * With the existence of this implicit code like the following simpley works:
   * {{{
   * val l = "Make true" ~> true
   * val r = mapValue(l){w=> "Got True"}
   * // r == Some("Got True")
   * }}}
   *
   * There is (currently) a downside of having this implicit. For reasons that
   * are not known (to me), Tuple pattern matching of Logger in for
   * comprehensions does not work if this implicit is in scope:
   * {{{
   *  // With getLoggerValue in scope:
   *  val tupleLogger = "Make true" ~> (44, "Hi", List(1,2,3))
   *  val l = for (
   *           (n, s, l) <- tupleLogger
   *         ) yield (n, s)
   *  // l == Some((44, "Hi"))
   *  // l.get == (44, "Hi")
   *
   *  // Without getLoggerValue in scope:
   *  val tupleLogger = "Make true" ~> (44, "Hi", List(1,2,3))
   *  val l = for (
   *           t <- tupleLogger
   *         ) yield (t._1, t._2)
   *  // l == Logger(List(Make true),Some((44,Hi)))
   *  // l.get == Value((44, "Hi"))
   *
   *  // This is a reasonable workaround
   *  val tupleLogger = "Make true" ~> (44, "Hi", List(1,2,3))
   *  val l = for (
   *           t <- tupleLogger;
   *           (n, s, l) <- t
   *         ) yield (n, s)
   *  // l == Logger(List(Make true),Some((44,Hi)))
   *  // l.get == Value((44, "Hi"))
   * }}}
   * 
   */
  implicit def getLoggerValue[E,C,L,W](l: Logger[E,C,L,W]): Option[W] = l.getValue

  def mapValue[W,R](wOp: Option[W])(f:W=>R): Option[R] = wOp.map{w=>f(w)}

  /////////////////////////////////////////////////////////////////////////////

  // Explicit String  
  // Logger(m.empty,wrapped(a))
  implicit def logString[L: TypeTag,W: TypeTag](w: W) =
    Logger.unital[String,List[String],L,W](w)

  /////////////////////////////////////////////////////////////////////////////
  class OpsList[E,C,L: TypeTag,W: TypeTag](ls1: List[Logger[E,C,L,W]])(implicit m: Monad[E,C]) {
    def ~>[V: TypeTag](l2: Logger[E,C,L,V]): Logger[E,C,L,V] = {
      var logList = for (ls <- ls1) yield ls.log
      val logs = m.flatten(logList)
      Logger[E,C,L,V](m.concat(logs, l2.log), l2.wrapped)
    }

    def doflatten: Logger[E,C,L,List[W]] = {
      
      ls1.find(ls => ls.hasError ) match {
        case Some(ls) => ls.generateErrorLogger[List[W]]
        case None =>
          var logList = for (ls <- ls1) yield ls.log
          val logs = m.flatten(logList)
          
          val valueList: List[W] = for (ls <- ls1) yield ls.getValue.get.asInstanceOf[W]
          val wrapped: Wrapped[L,List[W]] = new Union[UnionTOE[L,List[W]]#apply]
          wrapped.assign(valueList)
          Logger[E,C,L,List[W]](logs, wrapped)
      }
    }
  }
  implicit def getOpsList[E,C,L: TypeTag,W: TypeTag](ls: List[Logger[E,C,L,W]])(implicit m: Monad[E,C]) = 
      new OpsList[E,C,L,W](ls)

  /////////////////////////////////////////////////////////////////////////////
  class OpsListList[E,C,L: TypeTag,W: TypeTag](ls1: List[Logger[E,C,L,List[W]]])(implicit m: Monad[E,C]) {
    def doflatten: Logger[E,C,L,List[W]] = {
      ls1.find(ls => ls.hasError ) match {
        case Some(ls) => ls.generateErrorLogger[List[W]]
        case None =>
          var logList = for (ls <- ls1) yield ls.log
          val logs = m.flatten(logList)
      
          val valueListList: List[List[W]] = for (ls <- ls1) yield ls.getValue.get.asInstanceOf[List[W]]
          val valueList: List[W] = valueListList.flatten

          val wrapped: Wrapped[L,List[W]] = new Union[UnionTOE[L,List[W]]#apply]
          wrapped.assign(valueList)
          Logger[E,C,L,List[W]](logs, wrapped)
      }
    }
  }
  implicit def getOpsListList[E,C,L: TypeTag,W: TypeTag](ls: List[Logger[E,C,L,List[W]]])(implicit m: Monad[E,C]) = 
      new OpsListList[E,C,L,W](ls)



  /////////////////////////////////////////////////////////////////////////////
  class OpsC[E,C,L: TypeTag,W: TypeTag](l1: Logger[E,C,L,List[W]])(implicit m: Monad[E,C]) {
    def ++(l2: Logger[E,C,L,List[W]]): Logger[E,C,L,List[W]] = {
      if (l1.hasError) l1
      else if (l2.hasError) l2
      else {
        val listOfWrappedValue1: List[W]  = l1.getValue.get.asInstanceOf[List[W]]
        val listOfWrappedValue2: List[W]  = l2.getValue.get.asInstanceOf[List[W]]
        val wrapped: Wrapped[L,List[W]] = new Union[UnionTOE[L,List[W]]#apply]
        wrapped.assign(listOfWrappedValue1 ++ listOfWrappedValue2)
        Logger[E,C,L,List[W]](m.concat(l1.log,l2.log), wrapped)
      }
    }
  }

  implicit def getOpsC[E,C,L: TypeTag,W: TypeTag](l: Logger[E,C,L,List[W]])(implicit m: Monad[E,C]) = 
      new OpsC[E,C,L,W](l)



  /////////////////////////////////////////////////////////////////////////////
  class Ops[E,C,L: TypeTag,W: TypeTag](l1: Logger[E,C,L,W])(implicit m: Monad[E,C]) {
    def ~>(e: E): Logger[E,C,L,W] = {
      Logger[E,C,L,W](m.prepend(l1.log, e), l1.wrapped)
    }

    //  Logger[W] op V WV=>V            == Logger[V]
    def op[V: TypeTag](v: V)(fn: Tuple2[W,V] => V): Logger[E,C,L,V] = {
      l1.getValue match {
        case Some(w) => 
          val vv = fn((w,v))
          val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
          wrapped.assign(vv)
          Logger[E,C,L,V](l1.log, wrapped)
        case None => 
          val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
          wrapped.assign(v)
          Logger[E,C,L,V](l1.log, wrapped)
      }
    }

    def op[V](other: Logger[E,C,L,V])(fn: Tuple2[W,V] => W): Logger[E,C,L,W] = {
      l1.getValue match {
        case Some(w) => other.getValue match {
          case Some(v) => 
            val x = fn((w,v))
            val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
            wrapped.assign(x)
            Logger[E,C,L,W](m.concat(other.log, l1.log), wrapped)
          case None => Logger[E,C,L,W](m.concat(other.log, l1.log), l1.wrapped)
        }
        case None => Logger[E,C,L,W](m.concat(other.log, l1.log), l1.wrapped)
      }
    }

  }

  implicit def getOps[E,C,L: TypeTag,W: TypeTag](l: Logger[E,C,L,W])(implicit m: Monad[E,C]) = 
      new Ops[E,C,L,W](l)



  /////////////////////////////////////////////////////////////////////////////
  // given a log element type e, generate a Logger with the value w.
  class OpsLogValue[E,C](e: E)(implicit m: Monad[E,C]) {
    def unit[L: TypeTag]: Logger[E,C,L,Unit] = {
      val wrapped: Wrapped[L,Unit] = new Union[UnionTOE[L,Unit]#apply]
      Logger[E,C,L,Unit](m.single(e), wrapped)
    }

    def ~>[L: TypeTag]: Logger[E,C,L,Unit] = {
      val wrapped: Wrapped[L,Unit] = new Union[UnionTOE[L,Unit]#apply]
      Logger[E,C,L,Unit](m.single(e), wrapped)
    }

    def ~>[L: TypeTag,W: TypeTag](w: W): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(Some(w))
      Logger[E,C,L,W](m.single(e), wrapped)
    }
    def ~>[L: TypeTag,W: TypeTag](w: Option[W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(w)
      Logger[E,C,L,W](m.single(e), wrapped)
    }
    def ~>[L: TypeTag,W: TypeTag](w: Either[L,W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(w)
      Logger[E,C,L,W](m.single(e), wrapped)
    }
    def ~>[L: TypeTag,W: TypeTag](w: Try[W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(w)
      Logger[E,C,L,W](m.single(e), wrapped)
    }


    def ~>[L: TypeTag,W: TypeTag](l: Logger[E,C,L,W]): Logger[E,C,L,W] = {
      Logger[E,C,L,W](m.append(e, l.log), l.wrapped)
    }
  }

  implicit def stringListOpsLogValue(e: String) = 
    new OpsLogValue[String,List[String]](e)

  /////////////////////////////////////////////////////////////////////////////
  // given a value w, generate a Logger with the 
  // function mapping value to log element e.
  class OpsValueLog[L: TypeTag,W: TypeTag](w: W){
    def <~[E: TypeTag,C](
      k: W => E
    )(implicit m: Monad[E,C]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(Some(w))
      Logger(m.single(k(w)), wrapped)
    }
  }
  implicit def opsValueLog[L: TypeTag,W: TypeTag](w: W) = 
    new OpsValueLog[L,W](w)


  class OpsValueList[W: TypeTag](ws: List[W]){
    def dojoin[E,C,L: TypeTag](l: Logger[E,C,L,List[W]]): Logger[E,C,L,List[W]] = {
      if (l.hasError) l
      else {
        val listOfWrappedValue: List[W]  = l.getValue.get.asInstanceOf[List[W]]
        val wrapped: Wrapped[L,List[W]] = new Union[UnionTOE[L,List[W]]#apply]
        wrapped.assign(ws ++ listOfWrappedValue)
        Logger[E,C,L,List[W]](l.log, wrapped)
      }
    }
  }

  implicit def opsValueList[W: TypeTag](ws: List[W]) = 
    new OpsValueList[W](ws)
  /////////////////////////////////////////////////////////////////////////////

/*
  implicit def stringListOpsValueLog[L: TypeTag,W: TypeTag](w: W) = 
    new OpsValueLog[L,W](w)
*/



  /** Either Left value helper.
   *
   */
  case class Reason(kind: Int, msg: String, exceptionOp: Option[Exception] = None)

  object Generator {
    trait Base[E,C] {
      def as[L: TypeTag,W: TypeTag](value: W): Logger[E,C,L,W]
      def asMsg[L: TypeTag,W: TypeTag](e: E, value: W): Logger[E,C,L,W]
      def asMsgs[L: TypeTag,W: TypeTag](c: C, value: W): Logger[E,C,L,W]

      def asOption[L: TypeTag,W: TypeTag](value: Option[W]): Logger[E,C,L,W]
      def asOptionMsg[L: TypeTag,W: TypeTag](e: E, value: Option[W]): Logger[E,C,L,W]
      def asOptionMsgs[L: TypeTag,W: TypeTag](c: C, value: Option[W]): Logger[E,C,L,W]

      def asEither[L: TypeTag,W: TypeTag](value: Either[L,W]): Logger[E,C,L,W]
      def asEitherMsg[L: TypeTag,W: TypeTag](e: E, value: Either[L,W]): Logger[E,C,L,W]
      def asEitherMsgs[L: TypeTag,W: TypeTag](c: C, value: Either[L,W]): Logger[E,C,L,W]

      def asTry[L: TypeTag,W: TypeTag](value: Try[W]): Logger[E,C,L,W]
      def asTryMsg[L: TypeTag,W: TypeTag](e: E, value: Try[W]): Logger[E,C,L,W]
      def asTryMsgs[L: TypeTag,W: TypeTag](c: C, value: Try[W]): Logger[E,C,L,W]
    }

    object AsList {
      def apply[E]: AsList[E] = new AsList[E]
    }
    object StringAsList extends AsList[String]

    class AsList[E] extends Base[E,List[E]] {
      // val m = ListHelperPart.listHelperPart[E]
      val m = Monad.listMonad[E]

      def as[L: TypeTag,W: TypeTag](value: W): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger[E,List[E],L,W](m.empty, wrapper)
      }
      def asMsg[L: TypeTag,W: TypeTag](e: E, value: W): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.single(e), wrapper)
      }
      def asMsgs[L: TypeTag,W: TypeTag](c: List[E], value: W): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(c, wrapper)
      }

      def asOption[L: TypeTag,W: TypeTag](value: Option[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.empty, wrapper)
      }
      def asOptionMsg[L: TypeTag,W: TypeTag](e: E, value: Option[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.single(e), wrapper)
      }
      def asOptionMsgs[L: TypeTag,W: TypeTag](c: List[E], value: Option[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(c, wrapper)
      }

      def asEither[L: TypeTag, W: TypeTag](value: Either[L,W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.empty, wrapper)
      }
      def asEitherMsg[L: TypeTag, W: TypeTag](e: E, value: Either[L,W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.single(e), wrapper)
      }
      def asEitherMsgs[L: TypeTag, W: TypeTag](c: List[E], value: Either[L,W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(c, wrapper)
      }

      def asTry[L: TypeTag,W: TypeTag](value: Try[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.empty, wrapper)
      }
      def asTryMsg[L: TypeTag,W: TypeTag](e: E, value: Try[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(m.single(e), wrapper)
      }
      def asTryMsgs[L: TypeTag,W: TypeTag](c: List[E], value: Try[W]): Logger[E,List[E],L,W] = {
        val wrapper: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
        wrapper.assign(value)
        Logger(c, wrapper)
      }

    }

    def asStringList: Base[String,List[String]] = new AsList[String] {}
  }


  class LogClient[E,C,L: TypeTag](implicit m: Monad[E,C]) {
    var msgs: C = m.empty
    def apply(msg: E): Unit = msgs = m.append(msg, msgs) 
    def append(msg: E): Unit = msgs = m.append(msg, msgs) 
    def prepend(msg: E): Unit = msgs = m.prepend(msgs, msg) 
    def concat(c: C): Unit = msgs = m.concat(msgs, c) 
    def concat(l: Logger[E,C,_,_]): Unit = msgs = m.concat(msgs, l.log) 

// XXXXXXXXXXXXXXXXXXXXX
    def apply[B: TypeTag, W: TypeTag](
      logger: Logger[E,C,L,W]
    )(
      f: W => Logger[E,C,L,B]
    ): Logger[E,C,L,B] = { 
      concat(logger)
      val ll = Logger[E,C,L,W](m.empty, logger.wrapped)
      val l = ll.flatMap(f)
      concat(l)
      l.toEmpty
    }

    def apply[W: TypeTag](value: W): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(value)
      Logger[E,C,L,W](msgs, wrapped)
    }
    def apply[L: TypeTag, W: TypeTag](value: Option[W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(value)
      Logger[E,C,L,W](msgs, wrapped)
    }
    def apply[L: TypeTag, W: TypeTag](value: Either[L,W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(value)
      Logger[E,C,L,W](msgs, wrapped)
    }
    def apply[L: TypeTag, W: TypeTag](value: Try[W]): Logger[E,C,L,W] = {
      val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
      wrapped.assign(value)
      Logger[E,C,L,W](msgs, wrapped)
    }
  }

  /**
   * API
   *  item: value Option Either Try or Logger
   *  log(msg)
   *  log(item)
   *  log(msg,item)
   *  log(item,msg)
   *  log.~>(msg)
   *  log(item)
   *  log(msg,item)
   *  log(item,msg)
   */
/*
  class LogClientNew[E,C,L: TypeTag](implicit m: Monad[E,C]) {
    var wrapped: Wrapped[L,Any] = new Union[UnionTOE[L,Any]#apply]

    var msgs: C = m.empty
    def apply(msg: E): Unit = msgs = m.append(msg, msgs) 
    def append(msg: E): Unit = msgs = m.append(msg, msgs) 
    def prepend(msg: E): Unit = msgs = m.prepend(msgs, msg) 
    def concat(c: C): Unit = msgs = m.concat(msgs, c) 
    def concat(l: Logger[E,C,_,_]): Unit = msgs = m.concat(msgs, l.log) 

// XXXXXXXXXXXXXXXXXXXXX
/*
    def apply[B: TypeTag, W: TypeTag](
      logger: Logger[E,C,L,W]
    )(
      f: W => Logger[E,C,L,B]
    ): Logger[E,C,L,B] = { 
      concat(logger)
      val ll = Logger[E,C,L,W](m.empty, logger.wrapped)
      val l = ll.flatMap(f)
      concat(l)
      l.toEmpty
    }
*/

    def apply[W: TypeTag](msg: E, value: W): W = {
      if (! hasError) {
        msgs = m.append(msg, msgs) 
        wrapped.assign(value)
      }
      value
    }
    def apply[W: TypeTag](value: W): W = {
      if (! hasError) {
        wrapped.assign(value)
      }
      value
    }

    def apply[L: TypeTag, W: TypeTag](value: Option[W]): Option[W] = {
      if (! hasError) {
        wrapped.assign(value)
      }
      value
    }
    def apply[L: TypeTag, W: TypeTag](value: Either[L,W]): Either[L,W] = {
      if (! hasError) {
        wrapped.assign(value)
      }
      value
    }
    def apply[L: TypeTag, W: TypeTag](value: Try[W]): Try[W] = {
      if (! hasError) {
        wrapped.assign(value)
      }
      value
    }

    def apply[L: TypeTag, W: TypeTag](logger: Logger[E,C,L,W]): Option[W] = {
      if (! hasError) {
        msgs = m.concat(logger.log, msgs) 
        wrapped.assign(logger.getRaw)
      }
      logger.getValue map { w => w.asInstanceOf[W] }
    }

    def hasError: Boolean = wrapped.rawValue match {
      case Some(_) => false
      case None => true
      case Right(_) => false
      case Left(_) => true
      case Success(_) => false
      case Failure(_) => true
      case w => false
    }
  }
*/




  def enter_leave[E,C,L: TypeTag, T: TypeTag](
    code: => Logger[E,C,L,T]
  ) (implicit m: Monad[E,C], lh: LogHelper[E,C]): Logger[E,C,L,T] = {
    val enter = lh.enter
    try {
      val v: Logger[E,C,L,T] = code
      Logger[E,C,L,T](m.prepend(m.append(enter, v.log), lh.leave), v.wrapped)
    } catch {
      case scala.util.control.NonFatal(ex) =>
        val wrapped: Wrapped[L,T] = new Union[UnionTOE[L,T]#apply]
        wrapped.assign(Failure(ex))
        Logger[E,C,L,T](m.join(enter, lh.leave), wrapped)
    }
  }

/*
  def blockOld[E,C,L: TypeTag,T: TypeTag](
    code: LogClient[E,C,L] => T
  ) (implicit m: Monad[E,C]): Logger[E,C,L,T] = {
    val log = new LogClient[E,C,L]()
    val v = code(log)
    val wrapped: Wrapped[L,T] = new Union[UnionTOE[L,T]#apply]
    wrapped.assign(Some(v))
    Logger(log.msgs, wrapped)
  }
*/
  def block[E,C,L: TypeTag,T: TypeTag](
    code: LogClient[E,C,L] => Logger[E,C,L,T]
  ) (implicit m: Monad[E,C]): Logger[E,C,L,T] = {
    val logClient = new LogClient[E,C,L]()
    val l = code(logClient)
    Logger(logClient.msgs, l.wrapped)
  }

/*
  def blockNew[E,C,L: TypeTag,T: TypeTag](
    code: LogClientNew[E,C,L] => T
  ) (implicit m: Monad[E,C]): Logger[E,C,L,T] = {
    val logClient = new LogClientNew[E,C,L]()
    val t: T = code(logClient)
    if (logClient.hasError) {
      Logger[E,C,L,T](logClient.msgs, logClient.wrapped.asInstanceOf[Wrapped[L,T]])
    } else {
      val wrapped: Wrapped[L,T] = new Union[UnionTOE[L,T]#apply]
      wrapped.assign(t)
      Logger[E,C,L,T](logClient.msgs, wrapped)
    }
  }
// XXXXXXXXXXXXXXXXXXXXX
  def blockX[E,C,L: TypeTag,T: TypeTag](
    code: Logger[E,C,L,Unit] => Logger[E,C,L,T]
  ) (implicit m: Monad[E,C]): Logger[E,C,L,T] = {
    val wrapped: Wrapped[L,Unit] = new Union[UnionTOE[L,Unit]#apply]
    val logger =  Logger[E,C,L,Unit](m.empty, wrapped)
    code(logger)
  }
*/
}

import Logger._

/**
 * 
 * If one "iterates" over the container of type "C", one might expect
 * that the order is from earlies log message "E" to the last message.
 * This is a first-to-last, FIFO, temporal order.
 * But, this ordering would conflict with the case where the container
 * happens to be a List, C == List[E], when log messages are added
 * to the front, head, of the list rather than appending to the tail
 * of the list which is expensive.
 * Because it is reasonable to assume to assume that the container
 * as List will be the most common container type, the "expected"
 * order for an iteration will have to be from last to first, LIFO; 
 * a temporal ordering where the last message logged is the first
 * to appear during an iteration.
 * Calling reverse once at the end is much cheaper than always
 * appending.
 *
 *
 *
 * If your container happens to be a List, C == List[E], then there
 * is the issue that one is always appending to the tail of the list
 * which is expensive.
 * 
 * @define monadinfo an implicit value of class Helper which determines
 *    the Log Element/Container behavior. It is selected based upon Log 
 *    Element type (E) and Log Container type (C).
 *
 */
case class Logger[E,C,L: TypeTag,W: TypeTag](log: C, wrapped: Wrapped[L,W]) {

// XXXXXXXXXXXXXXXXXXXXX

  override def toString: String = {
    "Logger(" + log + "," + getRaw + ")"
  }


  // YYYYYYYYYYYYYYYYY
  def getRaw: Any = wrapped.rawValue

  def get: Node[W] = wrapped.rawValue match {
      case Some(w) => Value(w.asInstanceOf[W])
      case None => OptionError
      case Right(w) => Value(w.asInstanceOf[W])
      case Left(s) => EitherError(s.asInstanceOf[L])
      case Success(w) => Value(w.asInstanceOf[W])
      case Failure(ex) => TryError(ex)
      case w => Value(w.asInstanceOf[W])
    }

  def getValue: Option[W] = wrapped.rawValue match {
      case Some(w) => Some(w.asInstanceOf[W])
      case None => None
      case Right(w) => Some(w.asInstanceOf[W])
      case Left(_) => None
      case Success(w) => Some(w.asInstanceOf[W])
      case Failure(_) => None
      case w => Some(w.asInstanceOf[W])
    }

  def hasValue: Boolean = wrapped.rawValue match {
      case Some(_) => true
      case None => false
      case Right(_) => true
      case Left(_) => false
      case Success(_) => true
      case Failure(_) => false
      case w => true
    }
 
  def getError: Option[Node[W]] = wrapped.rawValue match {
      case Some(_) => None
      case None => Some(OptionError)
      case Right(_) => None
      case Left(s) => Some(EitherError(s.asInstanceOf[L]))
      case Success(_) => None
      case Failure(ex) => Some(TryError(ex))
      case w => None
    }

  def hasError: Boolean = wrapped.rawValue match {
      case Some(_) => false
      case None => true
      case Right(_) => false
      case Left(_) => true
      case Success(_) => false
      case Failure(_) => true
      case w => false
    }


  /** New Logger with current Wrapped Value but empty Log Container
   * 
   * @tparm E   Log Element type
   * @tparm C   Log Container type
   * @tparm L   Wrapped Value Either Left type
   * @tparm W   Wrapped Value type
   * @parm m    $monadinfo
   * @return    Empty Logger
   */
  def toEmpty(implicit m: Monad[E,C]) : Logger[E,C,L,W] = {
    Logger[E,C,L,W](m.empty, wrapped)
  }

  // add other's log to this log
  def concat(other: Logger[E,C,_,_])(implicit m: Monad[E,C]) : Logger[E,C,L,W] = {
    Logger[E,C,L,W](m.concat(log, other.log), wrapped)
  }

  def merge(others: List[Logger[E,C,_,_]])(implicit m: Monad[E,C]) : Logger[E,C,L,W] = {
    var r = for (other <- others) yield other.log
    val logs = m.flatten(r)
    Logger[E,C,L,W](m.concat(logs, log), wrapped)
  }


  /** Returns new Logger with value 'v' of type V and logs from from existing 
   * logger but if the existing logger has an error, then it is propagated 
   * rather an setting value with 'v'.
   * 
   * Here a Logger is created with value 44 with type Int and then the value 
   * is replace with "Hi" of type String.
   * {{{
   *  val l = "logger with value 44" ~> 44
   *  l.replace("Hi")
   * }}}
   *
   * @tparam V type of value
   * @param v value of new Logger
   * @return logger
   */
  def replace[V: TypeTag](v: V): Logger[E,C,L,V] = {
    if (hasError) generateErrorLogger
    else {
      val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
      wrapped.assign(v)
      Logger[E,C,L,V](log, wrapped)
    }
  }
  def replace[V: TypeTag](v: Option[V]): Logger[E,C,L,V] = {
    if (hasError) generateErrorLogger
    else {
      val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
      wrapped.assign(v)
      Logger[E,C,L,V](log, wrapped)
    }
  }
  def replace[V: TypeTag](v: Either[L,V]): Logger[E,C,L,V] = {
    if (hasError) generateErrorLogger
    else {
      val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
      wrapped.assign(v)
      Logger[E,C,L,V](log, wrapped)
    }
  }
  def replace[V: TypeTag](v: Try[V]): Logger[E,C,L,V] = {
    if (hasError) generateErrorLogger
    else {
      val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
      wrapped.assign(v)
      Logger[E,C,L,V](log, wrapped)
    }
  }

 //  Logger[_] replace Logger[W]     == Logger[W]
  def replace[V: TypeTag](l2: Logger[E,C,L,V])(implicit m: Monad[E,C]): Logger[E,C,L,V] = {
    if (hasError) generateErrorLogger
    else Logger[E,C,L,V](m.concat(log,l2.log), l2.wrapped)
  }

  protected def generateErrorLogger[V: TypeTag]: Logger[E,C,L,V] = {
    val wrapped: Wrapped[L,V] = new Union[UnionTOE[L,V]#apply]
    getError match {
      case Some(OptionError) => wrapped.assign(None)
      case Some(EitherError(s)) => wrapped.assign(Left(s.asInstanceOf[L]))
      case Some(TryError(ex)) => wrapped.assign(Failure(ex))
      case None | Some(_) => // should not happen
        wrapped.assign(Failure(new Exception("Should not happen")))
    }
    Logger[E,C,L,V](log, wrapped)
  }

/*
  def op[V](other: Logger[E,C,L,V], op: (V,W) => W)(implicit m: Monad[E,C]) : Logger[E,C,L,W] = {
    getValue match {
      case Some(w) => other.getValue match {
        case Some(v) => 
          val x = op(v,w)
          val wrapped: Wrapped[L,W] = new Union[UnionTOE[L,W]#apply]
          wrapped.assign(x)
          Logger[E,C,L,W](m.concat(log, other.log), wrapped)
        case None =>
          Logger[E,C,L,W](m.concat(log, other.log), wrapped)
      }
      case None =>
        Logger[E,C,L,W](m.concat(log, other.log), wrapped)
    }
  }
*/





  def filter(f: W => Boolean): Logger[E,C,L,W] = {
    this
  }

  def foreach[B](f: W => B): Unit = {
    val v = wrapped.rawValue
    if (v.isInstanceOf[Option[W]]) {
      val aOp = v.asInstanceOf[Option[W]]
      aOp match {
        case Some(a) => f(a)
        case None =>
      }

    } else if (v.isInstanceOf[Either[L, W]]) {
      val aE = v.asInstanceOf[Either[L, W]]
      aE match {
        case Right(a) => f(a)
        case Left(a) =>
      }

    } else if (v.isInstanceOf[Try[W]]) {
      val aT = v.asInstanceOf[Try[W]]
      aT match {
        case Success(a) => f(a)
        case Failure(ex) =>
      }

    } else {
      val a = v.asInstanceOf[W]
      f(a)
    }
  }

  def map[B: TypeTag](f: W => B): Logger[E,C,L,B] = {
    val v = wrapped.rawValue
    if (v.isInstanceOf[Option[W]]) {
      val aOp = v.asInstanceOf[Option[W]]
      aOp match {
        case Some(a) =>
          val b = f(a)
          val wrapped: Wrapped[L,B] = new Union[UnionTOE[L,B]#apply]
          wrapped.assign(Some(b))
          Logger(log, wrapped)
        case None =>
          val wrapped: Wrapped[L,Int] = new Union[UnionTOE[L,Int]#apply]
          wrapped.assign(None)
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }

    } else if (v.isInstanceOf[Either[L, W]]) {
      val aE = v.asInstanceOf[Either[L, W]]
      aE match {
        case Right(a) =>
          val b = f(a)
          val wrapped: Wrapped[L,B] = new Union[UnionTOE[L,B]#apply]
          wrapped.assign(Right(b))
          Logger(log, wrapped)
        case Left(a) =>
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }

    } else if (v.isInstanceOf[Try[W]]) {
      val aT = v.asInstanceOf[Try[W]]
      aT match {
        case Success(a) =>
          val b = f(a)
          val wrapped: Wrapped[L,B] = new Union[UnionTOE[L,B]#apply]
          wrapped.assign(Success(b))
          Logger(log, wrapped)
        case Failure(ex) =>
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }

    } else {
      val a = v.asInstanceOf[W]
      val b = f(a)
      val wrapped: Wrapped[L,B] = new Union[UnionTOE[L,B]#apply]
      wrapped.assign(b)
      Logger(log, wrapped)
    }
  }

  def flatMap[B: TypeTag](
    f: W => Logger[E,C,L,B]
  ) (implicit m: Monad[E,C]): Logger[E,C,L,B] = {
    val v = wrapped.rawValue
    if (v.isInstanceOf[Option[W]]) {
      val aOp = v.asInstanceOf[Option[W]]
      aOp match {
        case Some(a) =>
          val ll = f(a)
          Logger(m.concat(log, ll.log), ll.wrapped)
        case None =>
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }
    } else if (v.isInstanceOf[Either[_, W]]) {
      val aE = v.asInstanceOf[Either[String, W]]
      aE match {
        case Right(a) =>
          val ll = f(a)
          Logger(m.concat(log, ll.log), ll.wrapped)
        case Left(a) =>
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }
    } else if (v.isInstanceOf[Try[W]]) {
      val aT = v.asInstanceOf[Try[W]]
      aT match {
        case Success(a) =>
          val ll = f(a)
          Logger(m.concat(log, ll.log), ll.wrapped)
        case Failure(ex) =>
          Logger(log, wrapped.asInstanceOf[Wrapped[L,B]])
      }
    } else {
      val a = v.asInstanceOf[W]
      val ll = f(a)
      Logger(m.concat(log, ll.log), ll.wrapped)
    }
  }
}
