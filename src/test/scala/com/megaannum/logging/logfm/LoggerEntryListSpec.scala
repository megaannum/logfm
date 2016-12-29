package com.megaannum.logging.logfm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}
import scala.util.{Either, Left, Right}

import com.megaannum.logging.util.Union
import com.megaannum.logging.logfm.Logger._
import com.megaannum.logging.logfm.Log.Level

class LoggerEntryListSpec extends FlatSpec with Matchers {

  class Entry(
    val level: Log.Level.Type,
    msgfn: => String,
    val exceptionOp: Option[Exception] = None
  ) {
    val timeStamp: Long = System.currentTimeMillis
    def msg: String = msgfn

    override def equals(other: Any): Boolean = other match {
      case that: Entry =>
        that.level == this.level &&
        that.msg == this.msg &&
        that.exceptionOp == this.exceptionOp &&
        that.timeStamp == this.timeStamp
      case _ => false
    }
    override def toString(): String = {
      s"""Entry(Level.$level,"$msg")"""
    }
  }
  object Entry {
    def apply(
      level: Log.Level.Type,
      msg: => String,
      exceptionOp: Option[Exception] = None
    ): Entry = new Entry(level, msg, exceptionOp)
  }

  val entry1 = Entry(Level.INFO, "Test one")
  val entry2 = Entry(Level.WARN, "Test two")

  val listGen = new Generator.AsList[Entry]

  type LOGGER[W] = Logger[Entry,List[Entry],String,W]

  def doAs[W: TypeTag](value: W): LOGGER[W] = {
    listGen.as(value)
  }

  def doAsMsg[W: TypeTag](msg: Entry, value: W): LOGGER[W] = {
    listGen.asMsg(msg, value)
  }
 
  def doAsMsgs[W: TypeTag](msgs: List[Entry], value: W): LOGGER[W] = {
    listGen.asMsgs(msgs, value)
  }

  def doAsOption[W: TypeTag](value: Option[W]): LOGGER[W] = {
    listGen.asOption(value)
  }
  def doAsOptionMsg[W: TypeTag](msg: Entry, value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsg(msg, value)
  }
  def doAsOptionMsgs[W: TypeTag](msgs: List[Entry], value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsgs(msgs, value)
  }
  
  def doAsEither[W: TypeTag](value: Either[String,W]): LOGGER[W] = {
    listGen.asEither(value)
  }
  def doAsEitherMsg[W: TypeTag](msg: Entry, value: Either[String,W]): LOGGER[W] = {
    listGen.asEitherMsg(msg, value)
  }
  def doAsEitherMsgs[W: TypeTag](msgs: List[Entry], value: Either[String,W]): LOGGER[W] = {
    listGen.asEitherMsgs(msgs, value)
  }

  def doAsTry[W: TypeTag](value: Try[W]): LOGGER[W] = {
    listGen.asTry(value)
  }
  def doAsTryMsg[W: TypeTag](msg: Entry, value: Try[W]): LOGGER[W] = {
    listGen.asTryMsg(msg, value)
  }
  def doAsTryMsgs[W: TypeTag](msgs: List[Entry], value: Try[W]): LOGGER[W] = {
    listGen.asTryMsgs(msgs, value)
  }

  // val logger: Logger[Entry,List[Entry],Int] = Logger.logString(44)

  it should "Log values equal" in {
    doAs(4).get should be (Value(4))
    doAs(4).getRaw should be (4)
    doAs(4).getValue should be (Some(4))
    doAs("Test 1").get should be (Value("Test 1"))
    doAs("Test 1").getRaw should be ("Test 1")
    doAs("Test 1").getValue should be (Some("Test 1"))
    doAs(List(4)).get should be (Value(List(4)))
    doAs(List(4)).getRaw should be (List(4))
    doAs(List(4)).getValue should be (Some(List(4)))
    doAs(List("Test 1")).get should be (Value(List("Test 1")))
    doAs(List("Test 1")).getRaw should be (List("Test 1"))
    doAs(List("Test 1")).getValue should be (Some(List("Test 1")))


    doAsMsg(entry1, 5).get should be (Value(5))
    doAsMsg(entry1, 5).getRaw should be (5)
    doAsMsg(entry1, 5).getValue should be (Some(5))
    doAsMsg(entry1, 5).log should be (List(entry1))
    doAsMsg(entry1, "Test 2").get should be (Value("Test 2"))
    doAsMsg(entry1, "Test 2").getRaw should be ("Test 2")
    doAsMsg(entry1, "Test 2").getValue should be (Some("Test 2"))
    doAsMsg(entry1, "Test 2").log should be (List(entry1))


    doAsMsgs(List(entry1), 6).get should be (Value(6))
    doAsMsgs(List(entry1), 6).getRaw should be (6)
    doAsMsgs(List(entry1), 6).getValue should be (Some(6))
    doAsMsgs(List(entry1), 6).log should be (List(entry1))
    doAsMsgs(List(entry1), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List(entry1), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List(entry1), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List(entry1), "Test 3").log should be (List(entry1))

    doAsMsgs(List(entry1,entry2), 6).get should be (Value(6))
    doAsMsgs(List(entry1,entry2), 6).getRaw should be (6)
    doAsMsgs(List(entry1,entry2), 6).getValue should be (Some(6))
    doAsMsgs(List(entry1,entry2), 6).log should be (List(entry1,entry2))
    doAsMsgs(List(entry1,entry2), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List(entry1,entry2), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List(entry1,entry2), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List(entry1,entry2), "Test 3").log should be (List(entry1,entry2))
  }

  it should "Log error values equal" in {
    doAs(4).hasError should be (false)
    doAs("Test 3").hasError should be (false)

    doAsMsg(entry1, 5).hasError should be (false)
    doAsMsg(entry1, "Test 3").hasError should be (false)

    doAsMsgs(List(entry1), 5).hasError should be (false)
    doAsMsgs(List(entry1), "Test 3").hasError should be (false)

    doAsMsgs(List(entry1, entry2), 5).hasError should be (false)
    doAsMsgs(List(entry1, entry2), "Test 3").hasError should be (false)
  }
  it should "Log Option values equal" in {
    doAsOption(None).get should be (OptionError)
    doAsOption(None).getRaw should be (None)
    doAsOption(None).getValue should be (None)
    doAsOption(Some(4)).get should be (Value(4))
    doAsOption(Some(4)).getRaw should be (Some(4))
    doAsOption(Some(4)).getValue should be (Some(4))
    doAsOption(Some("Test 1")).get should be (Value("Test 1"))
    doAsOption(Some("Test 1")).getRaw should be (Some("Test 1"))
    doAsOption(Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOption(Some(List(4))).get should be (Value(List(4)))
    doAsOption(Some(List(4))).getRaw should be (Some(List(4)))
    doAsOption(Some(List(4))).getValue should be (Some(List(4)))
    doAsOption(Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOption(Some(List("Test 1"))).getRaw should be (Some(List("Test 1")))
    doAsOption(Some(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsOptionMsg(entry1, None).get should be (OptionError)
    doAsOptionMsg(entry1, None).getValue should be (None)
    doAsOptionMsg(entry1, None).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(4)).get should be (Value(4))
    doAsOptionMsg(entry1, Some(4)).getValue should be (Some(4))
    doAsOptionMsg(entry1, Some(4)).log should be (List(entry1))
    doAsOptionMsg(entry1, Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsg(entry1, Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsg(entry1, Some("Test 1")).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsg(entry1, Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsg(entry1, Some(List(4))).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsg(entry1, Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsg(entry1, Some(List("Test 1"))).log should be (List(entry1))

    doAsOptionMsgs(List(entry1), None).get should be (OptionError)
    doAsOptionMsgs(List(entry1), None).getValue should be (None)
    doAsOptionMsgs(List(entry1), None).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List(entry1), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List(entry1), Some(4)).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List(entry1), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List(entry1), Some("Test 1")).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List(entry1), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List(entry1), Some(List(4))).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).log should be (List(entry1))

    doAsOptionMsgs(List(entry1,entry2), None).get should be (OptionError)
    doAsOptionMsgs(List(entry1,entry2), None).getValue should be (None)
    doAsOptionMsgs(List(entry1,entry2), None).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).log should be (List(entry1,entry2))
  }
  it should "Log Option error values equal" in {
    doAsOption(None).hasError should be (true)
    doAsOption(None).hasValue should be (false)
    doAsOption(Some(4)).hasError should be (false)
    doAsOption(Some("hi")).hasError should be (false)

    doAsOptionMsg(entry1, None).hasError should be (true)
    doAsOptionMsg(entry1, None).hasValue should be (false)
    doAsOptionMsg(entry1, Some(4)).hasError should be (false)
    doAsOptionMsg(entry1, Some("hi")).hasError should be (false)

    doAsOptionMsgs(List(entry1), None).hasError should be (true)
    doAsOptionMsgs(List(entry1), None).hasValue should be (false)
    doAsOptionMsgs(List(entry1), Some(4)).hasError should be (false)
    doAsOptionMsgs(List(entry1), Some("hi")).hasError should be (false)
  }

  it should "Log Either values equal" in {
    doAsEither(Left("A problem")).get should be (EitherError("A problem"))
    doAsEither(Left("A problem")).getValue should be (None)
    doAsEither(Right(4)).getValue should be (Some(4))
    doAsEither(Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEither(Right(List(4))).getValue should be (Some(List(4)))
    doAsEither(Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsg(entry2, Left("A problem")).get should be (EitherError("A problem"))
    doAsEitherMsg(entry2, Left("A problem")).getValue should be (None)
    doAsEitherMsg(entry2, Right(4)).getValue should be (Some(4))
    doAsEitherMsg(entry2, Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsg(entry2, Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsg(entry2, Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsgs(List(entry2), Left("A problem")).get should be (EitherError("A problem"))
    doAsEitherMsgs(List(entry2), Left("A problem")).getValue should be (None)
    doAsEitherMsgs(List(entry2), Right(4)).getValue should be (Some(4))
    doAsEitherMsgs(List(entry2), Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsgs(List(entry2), Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsgs(List(entry2), Right(List("Test 1"))).getValue should be (Some(List("Test 1")))
  }

  it should "Log Either error values equal" in {
    doAsEither(Left("Got Error")).hasError should be (true)
    doAsEither(Left("Got Error")).hasValue should be (false)
    doAsEither(Right(4)).hasError should be (false)
    doAsEither(Right(4)).hasValue should be (true)
    doAsEither(Right("hi")).hasError should be (false)
    doAsEither(Right("hi")).hasValue should be (true)

    doAsEitherMsg(entry1, Left("Got Error")).hasError should be (true)
    doAsEitherMsg(entry1, Left("Got Error")).hasValue should be (false)
    doAsEitherMsg(entry1, Right(4)).hasError should be (false)
    doAsEitherMsg(entry1, Right("hi")).hasError should be (false)

    doAsEitherMsgs(List(entry2), Left("Got Error")).hasError should be (true)
    doAsEitherMsgs(List(entry2), Left("Got Error")).hasValue should be (false)
    doAsEitherMsgs(List(entry2), Right(4)).hasError should be (false)
    doAsEitherMsgs(List(entry2), Right("hi")).hasError should be (false)
  }

  it should "Log try values equal" in {
    doAsTry(Success(4)).get should be (Value(4))
    doAsTry(Success(4)).getRaw should be (Success(4))
    doAsTry(Success(4)).getValue should be (Some(4))
    doAsTry(Success(4)).getError should be (None)
    doAsTry(Success(4)).hasError should be (false)
    doAsTry(Success(4)).hasValue should be (true)

    doAsTryMsg(entry1, Success("hi there")).get should be (Value("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getError should be (None)
    doAsTryMsg(entry1, Success("hi there")).hasError should be (false)
    doAsTryMsg(entry1, Success("hi there")).hasValue should be (true)

    doAsTryMsgs(List(entry2), Success("hi there")).get should be (Value("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getError should be (None)
    doAsTryMsgs(List(entry2), Success("hi there")).hasError should be (false)
    doAsTryMsgs(List(entry2), Success("hi there")).hasValue should be (true)
  }

  it should "Log error try values equal" in {
    val ex = new Exception("hi")
    doAsTry(Failure(ex)).get should be (TryError(ex))
    doAsTry(Failure(ex)).getRaw should be (Failure(ex))
    doAsTry(Failure(ex)).getValue should be (None)
    doAsTry(Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTry(Failure(ex)).hasValue should be (false)
    doAsTry(Failure(ex)).hasError should be (true)

    doAsTryMsg(entry1,Failure(ex)).get should be (TryError(ex))
    doAsTryMsg(entry1,Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsg(entry1,Failure(ex)).getValue should be (None)
    doAsTryMsg(entry1,Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsg(entry1,Failure(ex)).hasValue should be (false)
    doAsTryMsg(entry1,Failure(ex)).hasError should be (true)

    doAsTryMsgs(List(entry2),Failure(ex)).get should be (TryError(ex))
    doAsTryMsgs(List(entry2),Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsgs(List(entry2),Failure(ex)).getValue should be (None)
    doAsTryMsgs(List(entry2),Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsgs(List(entry2),Failure(ex)).hasValue should be (false)
    doAsTryMsgs(List(entry2),Failure(ex)).hasError should be (true)
  }
}
/**
 * Tests with String List of log elements and Message class as Either.Left
 */
class MessageEntryListSpec extends FlatSpec with Matchers {
  class Entry(
    val level: Log.Level.Type,
    msgfn: => String,
    val exceptionOp: Option[Exception] = None
  ) {
    val timeStamp: Long = System.currentTimeMillis
    def msg: String = msgfn

    override def equals(other: Any): Boolean = other match {
      case that: Entry =>
        that.level == this.level &&
        that.msg == this.msg &&
        that.exceptionOp == this.exceptionOp &&
        that.timeStamp == this.timeStamp
      case _ => false
    }
    override def toString(): String = {
      s"""Entry(Level.$level,"$msg")"""
    }
  }
  object Entry {
    def apply(
      level: Log.Level.Type,
      msg: => String,
      exceptionOp: Option[Exception] = None
    ): Entry = new Entry(level, msg, exceptionOp)
  }

  val entry1 = Entry(Level.INFO, "Test one")
  val entry2 = Entry(Level.WARN, "Test two")

  val listGen = new Generator.AsList[Entry]

  case class Message(msg: String)

  type LOGGER[W] = Logger[Entry,List[Entry],Message,W]
 
  def doAs[W: TypeTag](value: W): LOGGER[W] = {
    listGen.as(value)
  }
  def doAsMsg[W: TypeTag](msg: Entry, value: W): LOGGER[W] = {
    listGen.asMsg(msg, value)
  }
 
  def doAsMsgs[W: TypeTag](msgs: List[Entry], value: W): LOGGER[W] = {
    listGen.asMsgs(msgs, value)
  }

  def doAsOption[W: TypeTag](value: Option[W]): LOGGER[W] = {
    listGen.asOption(value)
  }
  def doAsOptionMsg[W: TypeTag](msg: Entry, value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsg(msg, value)
  }
  def doAsOptionMsgs[W: TypeTag](msgs: List[Entry], value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsgs(msgs, value)
  }
  
  def doAsEither[W: TypeTag](value: Either[Message,W]): LOGGER[W] = {
    listGen.asEither(value)
  }
  def doAsEitherMsg[W: TypeTag](msg: Entry, value: Either[Message,W]): LOGGER[W] = {
    listGen.asEitherMsg(msg, value)
  }
  def doAsEitherMsgs[W: TypeTag](msgs: List[Entry], value: Either[Message,W]): LOGGER[W] = {
    listGen.asEitherMsgs(msgs, value)
  }

  def doAsTry[W: TypeTag](value: Try[W]): LOGGER[W] = {
    listGen.asTry(value)
  }
  def doAsTryMsg[W: TypeTag](msg: Entry, value: Try[W]): LOGGER[W] = {
    listGen.asTryMsg(msg, value)
  }
  def doAsTryMsgs[W: TypeTag](msgs: List[Entry], value: Try[W]): LOGGER[W] = {
    listGen.asTryMsgs(msgs, value)
  }

  // val logger: Logger[Entry,List[Entry],Int] = Logger.logString(44)

  it should "Log values equal" in {
    doAs(4).get should be (Value(4))
    doAs(4).getRaw should be (4)
    doAs(4).getValue should be (Some(4))
    doAs("Test 1").get should be (Value("Test 1"))
    doAs("Test 1").getRaw should be ("Test 1")
    doAs("Test 1").getValue should be (Some("Test 1"))
    doAs(List(4)).get should be (Value(List(4)))
    doAs(List(4)).getRaw should be (List(4))
    doAs(List(4)).getValue should be (Some(List(4)))
    doAs(List("Test 1")).get should be (Value(List("Test 1")))
    doAs(List("Test 1")).getRaw should be (List("Test 1"))
    doAs(List("Test 1")).getValue should be (Some(List("Test 1")))


    doAsMsg(entry1, 5).get should be (Value(5))
    doAsMsg(entry1, 5).getRaw should be (5)
    doAsMsg(entry1, 5).getValue should be (Some(5))
    doAsMsg(entry1, 5).log should be (List(entry1))
    doAsMsg(entry1, "Test 2").get should be (Value("Test 2"))
    doAsMsg(entry1, "Test 2").getRaw should be ("Test 2")
    doAsMsg(entry1, "Test 2").getValue should be (Some("Test 2"))
    doAsMsg(entry1, "Test 2").log should be (List(entry1))


    doAsMsgs(List(entry1), 6).get should be (Value(6))
    doAsMsgs(List(entry1), 6).getRaw should be (6)
    doAsMsgs(List(entry1), 6).getValue should be (Some(6))
    doAsMsgs(List(entry1), 6).log should be (List(entry1))
    doAsMsgs(List(entry1), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List(entry1), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List(entry1), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List(entry1), "Test 3").log should be (List(entry1))

    doAsMsgs(List(entry1,entry2), 6).get should be (Value(6))
    doAsMsgs(List(entry1,entry2), 6).getRaw should be (6)
    doAsMsgs(List(entry1,entry2), 6).getValue should be (Some(6))
    doAsMsgs(List(entry1,entry2), 6).log should be (List(entry1,entry2))
    doAsMsgs(List(entry1,entry2), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List(entry1,entry2), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List(entry1,entry2), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List(entry1,entry2), "Test 3").log should be (List(entry1,entry2))
  }

  it should "Log error values equal" in {
    doAs(4).hasError should be (false)
    doAs("Test 3").hasError should be (false)

    doAsMsg(entry1, 5).hasError should be (false)
    doAsMsg(entry1, "Test 3").hasError should be (false)

    doAsMsgs(List(entry1), 5).hasError should be (false)
    doAsMsgs(List(entry1), "Test 3").hasError should be (false)

    doAsMsgs(List(entry1, entry2), 5).hasError should be (false)
    doAsMsgs(List(entry1, entry2), "Test 3").hasError should be (false)
  }

  it should "Log Option values equal" in {
    doAsOption(None).get should be (OptionError)
    doAsOption(None).getRaw should be (None)
    doAsOption(None).getValue should be (None)
    doAsOption(Some(4)).get should be (Value(4))
    doAsOption(Some(4)).getRaw should be (Some(4))
    doAsOption(Some(4)).getValue should be (Some(4))
    doAsOption(Some("Test 1")).get should be (Value("Test 1"))
    doAsOption(Some("Test 1")).getRaw should be (Some("Test 1"))
    doAsOption(Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOption(Some(List(4))).get should be (Value(List(4)))
    doAsOption(Some(List(4))).getRaw should be (Some(List(4)))
    doAsOption(Some(List(4))).getValue should be (Some(List(4)))
    doAsOption(Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOption(Some(List("Test 1"))).getRaw should be (Some(List("Test 1")))
    doAsOption(Some(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsOptionMsg(entry1, None).get should be (OptionError)
    doAsOptionMsg(entry1, None).getValue should be (None)
    doAsOptionMsg(entry1, None).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(4)).get should be (Value(4))
    doAsOptionMsg(entry1, Some(4)).getValue should be (Some(4))
    doAsOptionMsg(entry1, Some(4)).log should be (List(entry1))
    doAsOptionMsg(entry1, Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsg(entry1, Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsg(entry1, Some("Test 1")).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsg(entry1, Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsg(entry1, Some(List(4))).log should be (List(entry1))
    doAsOptionMsg(entry1, Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsg(entry1, Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsg(entry1, Some(List("Test 1"))).log should be (List(entry1))

    doAsOptionMsgs(List(entry1), None).get should be (OptionError)
    doAsOptionMsgs(List(entry1), None).getValue should be (None)
    doAsOptionMsgs(List(entry1), None).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List(entry1), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List(entry1), Some(4)).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List(entry1), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List(entry1), Some("Test 1")).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List(entry1), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List(entry1), Some(List(4))).log should be (List(entry1))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List(entry1), Some(List("Test 1"))).log should be (List(entry1))

    doAsOptionMsgs(List(entry1,entry2), None).get should be (OptionError)
    doAsOptionMsgs(List(entry1,entry2), None).getValue should be (None)
    doAsOptionMsgs(List(entry1,entry2), None).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List(entry1,entry2), Some(4)).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List(entry1,entry2), Some("Test 1")).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List(entry1,entry2), Some(List(4))).log should be (List(entry1,entry2))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List(entry1,entry2), Some(List("Test 1"))).log should be (List(entry1,entry2))
  }
  it should "Log Option error values equal" in {
    doAsOption(None).hasError should be (true)
    doAsOption(None).hasValue should be (false)
    doAsOption(Some(4)).hasError should be (false)
    doAsOption(Some("hi")).hasError should be (false)

    doAsOptionMsg(entry1, None).hasError should be (true)
    doAsOptionMsg(entry1, None).hasValue should be (false)
    doAsOptionMsg(entry1, Some(4)).hasError should be (false)
    doAsOptionMsg(entry1, Some("hi")).hasError should be (false)

    doAsOptionMsgs(List(entry1), None).hasError should be (true)
    doAsOptionMsgs(List(entry1), None).hasValue should be (false)
    doAsOptionMsgs(List(entry1), Some(4)).hasError should be (false)
    doAsOptionMsgs(List(entry1), Some("hi")).hasError should be (false)
  }

  it should "Log Either values equal" in {
    doAsEither(Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEither(Left(Message("A problem"))).getValue should be (None)
    doAsEither(Right(4)).getValue should be (Some(4))
    doAsEither(Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEither(Right(List(4))).getValue should be (Some(List(4)))
    doAsEither(Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsg(entry2, Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEitherMsg(entry2, Left(Message("A problem"))).getValue should be (None)
    doAsEitherMsg(entry2, Right(4)).getValue should be (Some(4))
    doAsEitherMsg(entry2, Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsg(entry2, Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsg(entry2, Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsgs(List(entry2), Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEitherMsgs(List(entry2), Left(Message("A problem"))).getValue should be (None)
    doAsEitherMsgs(List(entry2), Right(4)).getValue should be (Some(4))
    doAsEitherMsgs(List(entry2), Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsgs(List(entry2), Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsgs(List(entry2), Right(List("Test 1"))).getValue should be (Some(List("Test 1")))
  }

  it should "Log Either error values equal" in {
    doAsEither(Left(Message("Got Error"))).hasError should be (true)
    doAsEither(Left(Message("Got Error"))).hasValue should be (false)
    doAsEither(Right(4)).hasError should be (false)
    doAsEither(Right(4)).hasValue should be (true)
    doAsEither(Right("hi")).hasError should be (false)
    doAsEither(Right("hi")).hasValue should be (true)

    doAsEitherMsg(entry1, Left(Message("Got Error"))).hasError should be (true)
    doAsEitherMsg(entry1, Left(Message("Got Error"))).hasValue should be (false)
    doAsEitherMsg(entry1, Right(4)).hasError should be (false)
    doAsEitherMsg(entry1, Right("hi")).hasError should be (false)

    doAsEitherMsgs(List(entry2), Left(Message("Got Error"))).hasError should be (true)
    doAsEitherMsgs(List(entry2), Left(Message("Got Error"))).hasValue should be (false)
    doAsEitherMsgs(List(entry2), Right(4)).hasError should be (false)
    doAsEitherMsgs(List(entry2), Right("hi")).hasError should be (false)
  }

  it should "Log try values equal" in {
    doAsTry(Success(4)).get should be (Value(4))
    doAsTry(Success(4)).getRaw should be (Success(4))
    doAsTry(Success(4)).getValue should be (Some(4))
    doAsTry(Success(4)).getError should be (None)
    doAsTry(Success(4)).hasError should be (false)
    doAsTry(Success(4)).hasValue should be (true)

    doAsTryMsg(entry1, Success("hi there")).get should be (Value("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsg(entry1, Success("hi there")).getError should be (None)
    doAsTryMsg(entry1, Success("hi there")).hasError should be (false)
    doAsTryMsg(entry1, Success("hi there")).hasValue should be (true)

    doAsTryMsgs(List(entry2), Success("hi there")).get should be (Value("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsgs(List(entry2), Success("hi there")).getError should be (None)
    doAsTryMsgs(List(entry2), Success("hi there")).hasError should be (false)
    doAsTryMsgs(List(entry2), Success("hi there")).hasValue should be (true)
  }

  it should "Log error try values equal" in {
    val ex = new Exception("hi")
    doAsTry(Failure(ex)).get should be (TryError(ex))
    doAsTry(Failure(ex)).getRaw should be (Failure(ex))
    doAsTry(Failure(ex)).getValue should be (None)
    doAsTry(Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTry(Failure(ex)).hasValue should be (false)
    doAsTry(Failure(ex)).hasError should be (true)

    doAsTryMsg(entry1,Failure(ex)).get should be (TryError(ex))
    doAsTryMsg(entry1,Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsg(entry1,Failure(ex)).getValue should be (None)
    doAsTryMsg(entry1,Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsg(entry1,Failure(ex)).hasValue should be (false)
    doAsTryMsg(entry1,Failure(ex)).hasError should be (true)

    doAsTryMsgs(List(entry2),Failure(ex)).get should be (TryError(ex))
    doAsTryMsgs(List(entry2),Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsgs(List(entry2),Failure(ex)).getValue should be (None)
    doAsTryMsgs(List(entry2),Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsgs(List(entry2),Failure(ex)).hasValue should be (false)
    doAsTryMsgs(List(entry2),Failure(ex)).hasError should be (true)
  }
}

/**
 * Tests function calls with String and Entry
 */
class FunctionEntryListSpec extends FlatSpec with Matchers {
  class Entry(
    val level: Log.Level.Type,
    msgfn: => String,
    val exceptionOp: Option[Exception] = None
  ) {
    val timeStamp: Long = System.currentTimeMillis
    def msg: String = msgfn

    override def equals(other: Any): Boolean = other match {
      case that: Entry =>
        that.level == this.level &&
        that.msg == this.msg &&
        that.exceptionOp == this.exceptionOp // &&
        // that.timeStamp == this.timeStamp
      case _ => false
    }
    override def toString(): String = {
      s"""Entry(Level.$level,"$msg")"""
    }
  }
  object Entry {
    def apply(
      level: Log.Level.Type,
      msg: => String,
      exceptionOp: Option[Exception] = None
    ): Entry = new Entry(level, msg, exceptionOp)
  }

  val entry1 = Entry(Level.INFO, "Test one")
  val entry2 = Entry(Level.WARN, "Test two")

  val listGen = new Generator.AsList[Entry]
  import listGen._

  implicit def entryListLogHelper: LogHelper[Entry, List[Entry]] =
      new ListLogHelper[Entry] {
        def enter: Entry = Entry(Level.INFO, "ENTER")
        def leave: Entry = Entry(Level.INFO, "LEAVE")
      }

  implicit def entryListOpsLogValue(e: Entry) = new OpsLogValue[Entry,List[Entry]](e)


  type LOGGER[W] = Logger[Entry,List[Entry],String,W]

  def addOne(n: Int): LOGGER[Int] =
    (Entry(Level.INFO, s"addOne: to $n")) ~> (n + 1)

  def adding(n: Int, v: Int): LOGGER[Int] =
    (Entry(Level.INFO, s"adding: $n to $v")) ~> (n + v)

  def doOption(n: Int): LOGGER[Int] = {
    asOptionMsg(Entry(Level.INFO, s"doOption: $n minus 4"), Some(n-4))
  }

  def doEither(n: Int): LOGGER[Int] = {
    asEitherMsg(Entry(Level.INFO,s"doEither: $n plus 4"), Right(n+4))
  }

  def doTry(n: Int): LOGGER[Int] = {
    asTryMsg(Entry(Level.INFO,s"doTry: $n times 2"), Success(n*2))
  }

  def doEnterLeave(n: Int, v: Int): LOGGER[Int] = enter_leave {
    (Entry(Level.INFO,s"doEnterLeave: $n plus $v")) ~> (n + v)
  }

  def doBlock(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO,"doBlock Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b
      log(r)(i=>
        for(
          a <- adding(i, 22);
          b <- adding(a, 2)
         ) yield b
      )
    } finally {
      log.prepend(Entry(Level.INFO,"doBlock Bottom"))
    }
  }


  it should "Return value and log entry" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.get should be (Value(11))
    l1.log should be (List(Entry(Level.INFO,"addOne: to 10")))

    val l2 = for (a <- adding(in, 5)) yield a
    l2.get should be (Value(15))
    l2.log should be (List(Entry(Level.INFO, "adding: 10 to 5")))

    val l3 = for (a <- doOption(in)) yield a
    l3.get should be (Value(6))
    l3.log should be (List(Entry(Level.INFO, "doOption: 10 minus 4")))

    val l4 = for (a <- doEither(in)) yield a
    l4.get should be (Value(14))
    l4.log should be (List(Entry(Level.INFO, "doEither: 10 plus 4")))

    val l5 = for (a <- doTry(in)) yield a
    l5.get should be (Value(20))
    l5.log should be (List(Entry(Level.INFO, "doTry: 10 times 2")))

    val l6 = for (a <- doEnterLeave(in, 12)) yield a
    l6.get should be (Value(22))
    l6.log should be (List(Entry(Level.INFO, "ENTER"), Entry(Level.INFO, "doEnterLeave: 10 plus 12"), Entry(Level.INFO, "LEAVE")))

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield b
    l7.get should be (Value(16))
    l7.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"addOne: to 15")))

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a);
        c <- doOption(b);
        d <- doEither(c);
        e <- doTry(d)
      ) yield e
    l8.get should be (Value(32))
    l8.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"addOne: to 15"), Entry(Level.INFO,"doOption: 16 minus 4"), Entry(Level.INFO,"doEither: 12 plus 4"), Entry(Level.INFO,"doTry: 16 times 2")))

    val l9 = for (a <- doBlock(in)) yield a
    l9.get should be (Value(43))
    l9.log should be (List(Entry(Level.INFO,"doBlock Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.INFO,"adding: 14 to 5"), Entry(Level.INFO,"adding: 19 to 22"), Entry(Level.INFO,"adding: 41 to 2"), Entry(Level.INFO,"doBlock Bottom")))

  }
  it should "Log error values equal" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.hasError should be (false)

    val l2 = for (a <- adding(in, 5)) yield a
    l2.hasError should be (false)

    val l3 = for (a <- doOption(in)) yield a
    l3.hasError should be (false)

    val l4 = for (a <- doEither(in)) yield a
    l4.hasError should be (false)

    val l5 = for (a <- doTry(in)) yield a
    l5.hasError should be (false)

    val l6 = for (a <- doEnterLeave(in, 12)) yield a
    l6.hasError should be (false)

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield b
    l7.hasError should be (false)

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a);
        c <- doOption(b);
        d <- doEither(c);
        e <- doTry(d)
      ) yield e
    l8.hasError should be (false)

    val l9 = for (a <- doBlock(in)) yield a
    l9.hasError should be (false)
  }
}
/**
 * Tests function calls with Message and Entry
 */
class FunctionMessageEntryListSpec extends FlatSpec with Matchers {
  class Entry(
    val level: Log.Level.Type,
    msgfn: => String,
    val exceptionOp: Option[Exception] = None
  ) {
    val timeStamp: Long = System.currentTimeMillis
    def msg: String = msgfn

    override def equals(other: Any): Boolean = other match {
      case that: Entry =>
        that.level == this.level &&
        that.msg == this.msg &&
        that.exceptionOp == this.exceptionOp // &&
        // that.timeStamp == this.timeStamp
      case _ => false
    }
    override def toString(): String = {
      s"""Entry(Level.$level,"$msg")"""
    }
  }
  object Entry {
    def apply(
      level: Log.Level.Type,
      msg: => String,
      exceptionOp: Option[Exception] = None
    ): Entry = new Entry(level, msg, exceptionOp)
  }

  val listGen = new Generator.AsList[Entry]
  import listGen._

  // for Either.Left
  case class Message(msg: String)

  implicit def entryListLogHelper: LogHelper[Entry, List[Entry]] =
      new ListLogHelper[Entry] {
        def enter: Entry = Entry(Level.INFO, "ENTER")
        def leave: Entry = Entry(Level.INFO, "LEAVE")
      }

  implicit def entryListOpsLogValue(e: Entry) = new OpsLogValue[Entry,List[Entry]](e)


  type LOGGER[W] = Logger[Entry,List[Entry],Message,W]

  def addOne(n: Int): LOGGER[Int] =
    (Entry(Level.INFO, s"addOne: to $n")) ~> (n + 1)

  def adding(n: Int, v: Int): LOGGER[Int] =
    (Entry(Level.INFO, s"adding: $n to $v")) ~> (n + v)

  def doOption(n: Int): LOGGER[Int] = {
    asOptionMsg(Entry(Level.INFO, s"doOption: $n minus 4"), Some(n-4))
  }

  def doOptionError(n: Int): LOGGER[Int] = {
    asOptionMsg(Entry(Level.ERROR, s"doOptionError: $n minus 4"), None)
  }

  def doEither(n: Int): LOGGER[Int] = {
    asEitherMsg(Entry(Level.INFO,s"doEither: $n plus 4"), Right(n+4))
  }

  def doEitherError(n: Int): LOGGER[Int] = {
    asEitherMsg(Entry(Level.ERROR, s"doEitherError: $n plus 4"), Left(Message("Got Either Error")))
  }

  def doTry(n: Int): LOGGER[Int] = {
    asTryMsg(Entry(Level.INFO,s"doTry: $n times 2"), Success(n*2))
  }

  val ex = new Exception("Got Try Error")
  def doTryError(n: Int): LOGGER[Int] = {
    asTryMsg(Entry(Level.ERROR, s"doTryError: $n times 2"), Failure(ex))
  }

  def doEnterLeave(n: Int, v: Int): LOGGER[Int] = enter_leave {
    (Entry(Level.INFO,s"doEnterLeave: $n plus $v")) ~> (n + v)
  }

  def doEnterLeaveOptionError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asOptionMsg(Entry(Level.ERROR, s"doEnterLeaveOptionError: $n minus $v"), None)
  }
  def doEnterLeaveEitherError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asEitherMsg(Entry(Level.ERROR, s"doEnterLeaveEitherError: $n minus $v"), Left(Message("Got Either Error")))
  }
  def doEnterLeaveTryError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asTryMsg(Entry(Level.ERROR, s"doEnterLeaveTryError: $n minus $v"), Failure(ex))
  }

  def doBlockPlusFourValue(n: Int): LOGGER[Int] = block { log =>
    log(n+4)
  }
  def doBlockPlusFourEitherValue(n: Int): LOGGER[Int] = block { log =>
    log(Right(n+4))
  }
  def doBlockPlusFourEitherError(n: Int): LOGGER[Int] = block { log =>
    log(Left(Message("Bad")))
  }

  def doBlock(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO,"doBlock Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b
      log(r)(i=>
        for(
          a <- adding(i, 22);
          b <- adding(a, 2)
         ) yield b
      )
    } finally {
      log.prepend(Entry(Level.INFO,"doBlock Bottom"))
    }
  }

  def doBlockOption1Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockOption1Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- doOptionError(a)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          c <- adding(a, a)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockOption1Error Bottom"))
    }
  }

  def doBlockOption2Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockOption2Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          b <- doOptionError(a);
          c <- adding(a, b)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockOption2Error Bottom"))
    }
  }

  def doBlockEither1Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockEither1Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- doEitherError(a)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          c <- adding(a, a)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockEither1Error Bottom"))
    }
  }

  def doBlockEither2Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockEither2Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          b <- doEitherError(a);
          c <- adding(a, b)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockEither2Error Bottom"))
    }
  }

  def doBlockTry1Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockTry1Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- doTryError(a)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          c <- adding(a, a)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockTry1Error Bottom"))
    }
  }

  def doBlockTry2Error(n: Int): LOGGER[Int] = block { log =>
    log(Entry(Level.INFO, "doBlockTry2Error Top"))
    try {
      val r =
        for(
          a <- adding(n, 4);
          b <- adding(a, 5)
         ) yield b

      log(r){i=>
        for(
          a <- adding(i, 22);
          b <- doTryError(a);
          c <- adding(a, b)
        ) yield c
      }
    } finally {
      log.prepend(Entry(Level.INFO, "doBlockTry2Error Bottom"))
    }
  }


  it should "Return value and log entry" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.get should be (Value(11))
    l1.log should be (List(Entry(Level.INFO,"addOne: to 10")))

    val l2 = for (a <- adding(in, 5)) yield a
    l2.get should be (Value(15))
    l2.log should be (List(Entry(Level.INFO, "adding: 10 to 5")))

    val l3 = for (a <- doOption(in)) yield a
    l3.get should be (Value(6))
    l3.log should be (List(Entry(Level.INFO, "doOption: 10 minus 4")))

    val l4 = for (a <- doEither(in)) yield a
    l4.get should be (Value(14))
    l4.log should be (List(Entry(Level.INFO, "doEither: 10 plus 4")))

    val l5 = for (a <- doTry(in)) yield a
    l5.get should be (Value(20))
    l5.log should be (List(Entry(Level.INFO, "doTry: 10 times 2")))

    val l6 = for (a <- doEnterLeave(in, 12)) yield a
    l6.get should be (Value(22))
    l6.log should be (List(Entry(Level.INFO, "ENTER"), Entry(Level.INFO, "doEnterLeave: 10 plus 12"), Entry(Level.INFO, "LEAVE")))

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield b
    l7.get should be (Value(16))
    l7.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"addOne: to 15")))

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a);
        c <- doOption(b);
        d <- doEither(c);
        e <- doTry(d)
      ) yield e
    l8.get should be (Value(32))
    l8.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"addOne: to 15"), Entry(Level.INFO,"doOption: 16 minus 4"), Entry(Level.INFO,"doEither: 12 plus 4"), Entry(Level.INFO,"doTry: 16 times 2")))

    val l9 = for (a <- doBlock(in)) yield a
    l9.get should be (Value(43))
    l9.log should be (List(Entry(Level.INFO,"doBlock Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.INFO,"adding: 14 to 5"), Entry(Level.INFO,"adding: 19 to 22"), Entry(Level.INFO,"adding: 41 to 2"), Entry(Level.INFO,"doBlock Bottom")))

    val l10 = for (a <- doBlockPlusFourValue(in)) yield a
    l10.hasError should be (false)
    l10.get should be (Value(14))

    val l11 = for (a <- doBlockPlusFourEitherValue(in)) yield a
    l11.hasError should be (false)
    l11.get should be (Value(14))

    val l12 = for (a <- doBlockPlusFourEitherError(in)) yield a
    l12.hasError should be (true)
    l12.get should be (EitherError(Message("Bad")))
  }
  it should "Log error detected" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.hasError should be (false)

    val l2 = for (a <- adding(in, 5)) yield a
    l2.hasError should be (false)

    val l3 = for (a <- doOption(in)) yield a
    l3.hasError should be (false)

    val l4 = for (a <- doEither(in)) yield a
    l4.hasError should be (false)

    val l5 = for (a <- doTry(in)) yield a
    l5.hasError should be (false)

    val l6 = for (a <- doEnterLeave(in, 12)) yield a
    l6.hasError should be (false)

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield b
    l7.hasError should be (false)

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a);
        c <- doOption(b);
        d <- doEither(c);
        e <- doTry(d)
      ) yield e
    l8.hasError should be (false)

    val l9 = for (a <- doBlock(in)) yield a
    l9.hasError should be (false)
  }
  it should "Log all error values equal" in {
    val in = 10

    val l1 = for (a <- doOptionError(in)) yield a
    l1.hasError should be (true)
    l1.get should be (OptionError)
    l1.log should be (List(Entry(Level.ERROR,"doOptionError: 10 minus 4")))

    val l2 = for (a <- doEitherError(in)) yield a
    l2.hasError should be (true)
    l2.get should be (EitherError(Message("Got Either Error")))
    l2.log should be (List(Entry(Level.ERROR,"doEitherError: 10 plus 4")))

    val l3 = for (a <- doTryError(in)) yield a
    l3.hasError should be (true)
    l3.get should be (TryError(ex))
    l3.log should be (List(Entry(Level.ERROR,"doTryError: 10 times 2")))

    val l4 = 
      for (
        a <- adding(in, 5);
        b <- doOptionError(a)
      ) yield b
    l4.hasError should be (true)
    l4.get should be (OptionError)
    l4.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doOptionError: 15 minus 4")))

    val l5 = 
      for (
        a <- adding(in, 5);
        b <- doOptionError(a);
        c <- addOne(b)
      ) yield c
    l5.hasError should be (true)
    l5.get should be (OptionError)
    l5.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doOptionError: 15 minus 4")))

    val l6 = 
      for (
        a <- adding(in, 5);
        b <- doEitherError(a)
      ) yield b
    l6.hasError should be (true)
    l6.get should be (EitherError(Message("Got Either Error")))
    l6.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doEitherError: 15 plus 4")))

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- doEitherError(a);
        c <- addOne(b)
      ) yield c
    l7.hasError should be (true)
    l7.get should be (EitherError(Message("Got Either Error")))
    l7.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doEitherError: 15 plus 4")))

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- doTryError(a)
      ) yield b
    l8.hasError should be (true)
    l8.get should be (TryError(ex))
    l8.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doTryError: 15 times 2")))

    val l9 = 
      for (
        a <- adding(in, 5);
        b <- doTryError(a);
        c <- addOne(b)
      ) yield c
    l9.hasError should be (true)
    l9.get should be (TryError(ex))
    l9.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.ERROR,"doTryError: 15 times 2")))

    val l10 = for (a <- doEnterLeaveOptionError(in, 1)) yield a
    l10.hasError should be (true)
    l10.get should be (OptionError)
    l10.log should be (List(Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveOptionError: 10 minus 1"), Entry(Level.INFO,"LEAVE")))

    val l11 = for (a <- doEnterLeaveEitherError(in, 1)) yield a
    l11.hasError should be (true)
    l11.get should be (EitherError(Message("Got Either Error")))
    l11.log should be (List(Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveEitherError: 10 minus 1"), Entry(Level.INFO,"LEAVE")))

    val l12 = for (a <- doEnterLeaveTryError(in, 1)) yield a
    l12.hasError should be (true)
    l12.get should be (TryError(ex))
    l12.log should be (List(Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveTryError: 10 minus 1"), Entry(Level.INFO,"LEAVE")))

    val l13 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveOptionError(a, 4);
        c <- addOne(b)
      ) yield c
    l13.hasError should be (true)
    l13.get should be (OptionError)
    l13.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveOptionError: 15 minus 4"), Entry(Level.INFO,"LEAVE")))

    val l14 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveEitherError(a, 4);
        c <- addOne(b)
      ) yield c
    l14.hasError should be (true)
    l14.get should be (EitherError(Message("Got Either Error")))
    l14.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveEitherError: 15 minus 4"), Entry(Level.INFO,"LEAVE")))

    val l15 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveTryError(a, 4);
        c <- addOne(b)
      ) yield c
    l15.hasError should be (true)
    l15.get should be (TryError(ex))
    l15.log should be (List(Entry(Level.INFO,"adding: 10 to 5"), Entry(Level.INFO,"ENTER"), Entry(Level.ERROR,"doEnterLeaveTryError: 15 minus 4"), Entry(Level.INFO,"LEAVE")))

    val l16 = for (a <- doBlockOption1Error(in)) yield a
    l16.hasError should be (true)
    l16.get should be (OptionError)
    l16.log should be (List(Entry(Level.INFO,"doBlockOption1Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.ERROR,"doOptionError: 14 minus 4"), Entry(Level.INFO,"doBlockOption1Error Bottom")))

    val l17 = for (a <- doBlockOption2Error(in)) yield a
    l17.hasError should be (true)
    l17.get should be (OptionError)
    l17.log should be (List(Entry(Level.INFO,"doBlockOption2Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.INFO,"adding: 14 to 5"), Entry(Level.INFO,"adding: 19 to 22"), Entry(Level.ERROR,"doOptionError: 41 minus 4"), Entry(Level.INFO,"doBlockOption2Error Bottom")))

    val l18 = for (a <- doBlockEither1Error(in)) yield a
    l18.hasError should be (true)
    l18.get should be (EitherError(Message("Got Either Error")))
    l18.log should be (List(Entry(Level.INFO,"doBlockEither1Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.ERROR,"doEitherError: 14 plus 4"), Entry(Level.INFO,"doBlockEither1Error Bottom")))

    val l19 = for (a <- doBlockEither2Error(in)) yield a
    l19.hasError should be (true)
    l19.get should be (EitherError(Message("Got Either Error")))
    l19.log should be (List(Entry(Level.INFO,"doBlockEither2Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.INFO,"adding: 14 to 5"), Entry(Level.INFO,"adding: 19 to 22"), Entry(Level.ERROR,"doEitherError: 41 plus 4"), Entry(Level.INFO,"doBlockEither2Error Bottom")))

    val l20 = for (a <- doBlockTry1Error(in)) yield a
    l20.hasError should be (true)
    l20.get should be (TryError(ex))
    l20.log should be (List(Entry(Level.INFO,"doBlockTry1Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.ERROR,"doTryError: 14 times 2"), Entry(Level.INFO,"doBlockTry1Error Bottom")))

    val l21 = for (a <- doBlockTry2Error(in)) yield a
    l21.hasError should be (true)
    l21.get should be (TryError(ex))
    l21.log should be (List(Entry(Level.INFO,"doBlockTry2Error Top"), Entry(Level.INFO,"adding: 10 to 4"), Entry(Level.INFO,"adding: 14 to 5"), Entry(Level.INFO,"adding: 19 to 22"), Entry(Level.ERROR,"doTryError: 41 times 2"), Entry(Level.INFO,"doBlockTry2Error Bottom")))
  }
}
