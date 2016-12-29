package com.megaannum.logging.logfm

import com.megaannum.logging.util.Union
import com.megaannum.logging.util.Union._
import scala.util.{Try, Success, Failure}
// import scala.util.{Either, Left, Right}
// import scala.reflect.runtime.universe._

object LoggerTest {
  import Logger._

  def main(args: Array[String]): Unit = {
    val x = args(0).toInt // parse int from command line
    val r =
      for(a <- addOne(x);
          b <- intString(a);
          c <- lengthIsEven(b);
          // d <- noLog(hundredOrThousand(c));
          d = hundredOrThousand(c);
          e <- times7(d);
          f <- dotry(e);
          g <- doeither(f);
          h <- doInt(g)
         ) yield h

    println("RAW: " + r.wrapped.rawValue)
    println("VALUE: " + r.getValue)
    val iOp: Option[Int] = r.wrapped.value[Int]
    println("RESULT: " + iOp)
    println
    println("LOG")
    println("---")
    r.log foreach println
  }
  def addOne(n: Int): Logger[String,List[String],String,Int] =
    ("adding one to " + n) ~> (n + 1)

  def adding(n: Int, v: Int): Logger[String,List[String],String,Int] =
    (s"adding $n to $v") ~> (n + v)

  def intStringX(n: Int): Logger[String,List[String],String,String] = {
    ("converting int to string " + n) ~> n.toString
  }
  def intString(n: Int): Logger[String,List[String],String,String] = enter_leave {
    ("converting int to string " + n) ~> n.toString
  }

  def lengthIsEven(s: String): Logger[String,List[String],String,Boolean] =
    ("checking length of " + s + " for evenness") ~> (s.length % 2 == 0)

  def hundredOrThousand(b: Boolean): Int = // no logging
    if(b) 100 else 1000

  def times7(n: Int): Logger[String,List[String],String,Int] =
    ((n * 7) <~ ("multiplying " + n + " by 7 to produce " + _)).flatMap { v =>
      (s"doing v=$v") ~> (v+2)
    }
  def dotry(n: Int): Logger[String,List[String],String,Int] = {
    val wrapped: Wrapped[String,Int] = new Union[UnionTOE[String,Int]#apply]
    wrapped.assign(Success(n+1))
    Logger("dotry plus 1"::Nil, wrapped)
  }
  def doeither(n: Int): Logger[String,List[String],String,Int] = {
    val wrapped: Wrapped[String,Int] = new Union[UnionTOE[String,Int]#apply]
    wrapped.assign(Right(n-4))
    Logger("doeither minus 4"::Nil, wrapped)
  }
  def doInt(n: Int): Logger[String,List[String],String,Int] = block { log =>
    log("doInt Top")
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b
      log(r)(i => 
        for(
          a <- adding(i, 4)
         ) yield a
      )
    } finally {
      log.prepend("doInt Bottom")
    }
  }
}
