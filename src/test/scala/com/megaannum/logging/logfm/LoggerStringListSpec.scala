package com.megaannum.logging.logfm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}
import scala.util.{Either, Left, Right}

import com.megaannum.logging.util.Union
import com.megaannum.logging.logfm.Logger._

/**
 * Tests with String List of log elements and String as Either.Left
 */
class LoggerStringStringListSpec extends FlatSpec with Matchers {

  val listGen = new Generator.AsList[String]

  type LOGGER[W] = Logger[String,List[String],String,W]
 
  def doAs[W: TypeTag](value: W): LOGGER[W] = {
    listGen.as(value)
  }
  def doAsMsg[W: TypeTag](msg: String, value: W): LOGGER[W] = {
    listGen.asMsg(msg, value)
  }
  def doAsMsgs[W: TypeTag](msgs: List[String], value: W): LOGGER[W] = {
    listGen.asMsgs(msgs, value)
  }
  def doAsOption[W: TypeTag](value: Option[W]): LOGGER[W] = {
    listGen.asOption(value)
  }
  def doAsOptionMsg[W: TypeTag](msg: String, value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsg(msg, value)
  }
  def doAsOptionMsgs[W: TypeTag](msgs: List[String], value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsgs(msgs, value)
  }
  
  def doAsEither[W: TypeTag](value: Either[String,W]): LOGGER[W] = {
    listGen.asEither(value)
  }
  def doAsEitherMsg[W: TypeTag](msg: String, value: Either[String,W]): LOGGER[W] = {
    listGen.asEitherMsg(msg, value)
  }
  def doAsEitherMsgs[W: TypeTag](msgs: List[String], value: Either[String,W]): LOGGER[W] = {
    listGen.asEitherMsgs(msgs, value)
  }

  def doAsTry[W: TypeTag](value: Try[W]): LOGGER[W] = {
    listGen.asTry(value)
  }
  def doAsTryMsg[W: TypeTag](msg: String, value: Try[W]): LOGGER[W] = {
    listGen.asTryMsg(msg, value)
  }
  def doAsTryMsgs[W: TypeTag](msgs: List[String], value: Try[W]): LOGGER[W] = {
    listGen.asTryMsgs(msgs, value)
  }

  // val logger: Logger[String,List[String],Int] = Logger.logString(44)


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

    doAsMsg("Msg 1", 5).get should be (Value(5))
    doAsMsg("Msg 1", 5).getRaw should be (5)
    doAsMsg("Msg 1", 5).getValue should be (Some(5))
    doAsMsg("Msg 1", 5).log should be (List("Msg 1"))
    doAsMsg("Msg 1", "Test 2").get should be (Value("Test 2"))
    doAsMsg("Msg 1", "Test 2").getRaw should be ("Test 2")
    doAsMsg("Msg 1", "Test 2").getValue should be (Some("Test 2"))
    doAsMsg("Msg 1", "Test 2").log should be (List("Msg 1"))

    doAsMsgs(List("Msg 1"), 6).get should be (Value(6))
    doAsMsgs(List("Msg 1"), 6).getRaw should be (6)
    doAsMsgs(List("Msg 1"), 6).getValue should be (Some(6))
    doAsMsgs(List("Msg 1"), 6).log should be (List("Msg 1"))
    doAsMsgs(List("Msg 1"), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List("Msg 1"), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List("Msg 1"), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List("Msg 1"), "Test 3").log should be (List("Msg 1"))

    doAsMsgs(List("Msg 1","Msg 2"), 6).get should be (Value(6))
    doAsMsgs(List("Msg 1","Msg 2"), 6).getRaw should be (6)
    doAsMsgs(List("Msg 1","Msg 2"), 6).getValue should be (Some(6))
    doAsMsgs(List("Msg 1","Msg 2"), 6).log should be (List("Msg 1","Msg 2"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").log should be (List("Msg 1","Msg 2"))
  }

  it should "Log error values equal" in {
    doAs(4).hasError should be (false)
    doAs("Test 3").hasError should be (false)

    doAsMsg("Msg 1", 5).hasError should be (false)
    doAsMsg("Msg 1", "Test 3").hasError should be (false)

    doAsMsgs(List("Msg 1"), 5).hasError should be (false)
    doAsMsgs(List("Msg 1"), "Test 3").hasError should be (false)

    doAsMsgs(List("Msg 1", "Msg 2"), 5).hasError should be (false)
    doAsMsgs(List("Msg 1", "Msg 2"), "Test 3").hasError should be (false)
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

    doAsOptionMsg("Msg 2", None).get should be (OptionError)
    doAsOptionMsg("Msg 2", None).getValue should be (None)
    doAsOptionMsg("Msg 2", None).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(4)).get should be (Value(4))
    doAsOptionMsg("Msg 2", Some(4)).getValue should be (Some(4))
    doAsOptionMsg("Msg 2", Some(4)).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsg("Msg 2", Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsg("Msg 2", Some("Test 1")).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsg("Msg 2", Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsg("Msg 2", Some(List(4))).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).log should be (List("Msg 2"))

    doAsOptionMsgs(List("Msg 2"), None).get should be (OptionError)
    doAsOptionMsgs(List("Msg 2"), None).getValue should be (None)
    doAsOptionMsgs(List("Msg 2"), None).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List("Msg 2"), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List("Msg 2"), Some(4)).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).log should be (List("Msg 2"))

    doAsOptionMsgs(List("Msg 2","Msg 3"), None).get should be (OptionError)
    doAsOptionMsgs(List("Msg 2","Msg 3"), None).getValue should be (None)
    doAsOptionMsgs(List("Msg 2","Msg 3"), None).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).log should be (List("Msg 2","Msg 3"))
  }
  it should "Log Option error values equal" in {
    doAsOption(None).hasError should be (true)
    doAsOption(None).hasValue should be (false)
    doAsOption(Some(4)).hasError should be (false)
    doAsOption(Some("hi")).hasError should be (false)

    doAsOptionMsg("Msg 3", None).hasError should be (true)
    doAsOptionMsg("Msg 3", None).hasValue should be (false)
    doAsOptionMsg("Msg 3", Some(4)).hasError should be (false)
    doAsOptionMsg("Msg 3", Some("hi")).hasError should be (false)

    doAsOptionMsgs(List("Msg 3"), None).hasError should be (true)
    doAsOptionMsgs(List("Msg 3"), None).hasValue should be (false)
    doAsOptionMsgs(List("Msg 3"), Some(4)).hasError should be (false)
    doAsOptionMsgs(List("Msg 3"), Some("hi")).hasError should be (false)
  }

  it should "Log Either values equal" in {
    doAsEither(Left("A problem")).get should be (EitherError("A problem"))
    doAsEither(Left("A problem")).getValue should be (None)
    doAsEither(Right(4)).getValue should be (Some(4))
    doAsEither(Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEither(Right(List(4))).getValue should be (Some(List(4)))
    doAsEither(Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsg("Msg 6", Left("A problem")).get should be (EitherError("A problem"))
    doAsEitherMsg("Msg 6", Left("A problem")).getValue should be (None)
    doAsEitherMsg("Msg 6", Right(4)).getValue should be (Some(4))
    doAsEitherMsg("Msg 6", Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsg("Msg 6", Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsg("Msg 6", Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsgs(List("Msg 6"), Left("A problem")).get should be (EitherError("A problem"))
    doAsEitherMsgs(List("Msg 6"), Left("A problem")).getValue should be (None)
    doAsEitherMsgs(List("Msg 6"), Right(4)).getValue should be (Some(4))
    doAsEitherMsgs(List("Msg 6"), Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsgs(List("Msg 6"), Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsgs(List("Msg 6"), Right(List("Test 1"))).getValue should be (Some(List("Test 1")))
  }

  it should "Log Either error values equal" in {
    doAsEither(Left("Got Error")).hasError should be (true)
    doAsEither(Left("Got Error")).hasValue should be (false)
    doAsEither(Right(4)).hasError should be (false)
    doAsEither(Right(4)).hasValue should be (true)
    doAsEither(Right("hi")).hasError should be (false)
    doAsEither(Right("hi")).hasValue should be (true)

    doAsEitherMsg("Msg 3", Left("Got Error")).hasError should be (true)
    doAsEitherMsg("Msg 3", Left("Got Error")).hasValue should be (false)
    doAsEitherMsg("Msg 3", Right(4)).hasError should be (false)
    doAsEitherMsg("Msg 3", Right("hi")).hasError should be (false)

    doAsEitherMsgs(List("Msg 3"), Left("Got Error")).hasError should be (true)
    doAsEitherMsgs(List("Msg 3"), Left("Got Error")).hasValue should be (false)
    doAsEitherMsgs(List("Msg 3"), Right(4)).hasError should be (false)
    doAsEitherMsgs(List("Msg 3"), Right("hi")).hasError should be (false)
  }

  it should "Log try values equal" in {
    doAsTry(Success(4)).get should be (Value(4))
    doAsTry(Success(4)).getRaw should be (Success(4))
    doAsTry(Success(4)).getValue should be (Some(4))
    doAsTry(Success(4)).getError should be (None)
    doAsTry(Success(4)).hasError should be (false)
    doAsTry(Success(4)).hasValue should be (true)

    doAsTryMsg("Msg 9", Success("hi there")).get should be (Value("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getError should be (None)
    doAsTryMsg("Msg 9", Success("hi there")).hasError should be (false)
    doAsTryMsg("Msg 9", Success("hi there")).hasValue should be (true)

    doAsTryMsgs(List("Msg 9"), Success("hi there")).get should be (Value("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getError should be (None)
    doAsTryMsgs(List("Msg 9"), Success("hi there")).hasError should be (false)
    doAsTryMsgs(List("Msg 9"), Success("hi there")).hasValue should be (true)
  }

  it should "Log error try values equal" in {
    val ex = new Exception("hi")
    doAsTry(Failure(ex)).get should be (TryError(ex))
    doAsTry(Failure(ex)).getRaw should be (Failure(ex))
    doAsTry(Failure(ex)).getValue should be (None)
    doAsTry(Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTry(Failure(ex)).hasValue should be (false)
    doAsTry(Failure(ex)).hasError should be (true)

    doAsTryMsg("Msg 9",Failure(ex)).get should be (TryError(ex))
    doAsTryMsg("Msg 9",Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsg("Msg 9",Failure(ex)).getValue should be (None)
    doAsTryMsg("Msg 9",Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsg("Msg 9",Failure(ex)).hasValue should be (false)
    doAsTryMsg("Msg 9",Failure(ex)).hasError should be (true)

    doAsTryMsgs(List("Msg 9"),Failure(ex)).get should be (TryError(ex))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getValue should be (None)
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).hasValue should be (false)
    doAsTryMsgs(List("Msg 9"),Failure(ex)).hasError should be (true)
  }

}

/**
 * Tests with String List of log elements and Message class as Either.Left
 */
class MessageStringListSpec extends FlatSpec with Matchers {

  val listGen = new Generator.AsList[String]

  case class Message(msg: String)

  type LOGGER[W] = Logger[String,List[String],Message,W]
 
  def doAs[W: TypeTag](value: W): LOGGER[W] = {
    listGen.as(value)
  }
  def doAsMsg[W: TypeTag](msg: String, value: W): LOGGER[W] = {
    listGen.asMsg(msg, value)
  }
  def doAsMsgs[W: TypeTag](msgs: List[String], value: W): LOGGER[W] = {
    listGen.asMsgs(msgs, value)
  }
  def doAsOption[W: TypeTag](value: Option[W]): LOGGER[W] = {
    listGen.asOption(value)
  }
  def doAsOptionMsg[W: TypeTag](msg: String, value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsg(msg, value)
  }
  def doAsOptionMsgs[W: TypeTag](msgs: List[String], value: Option[W]): LOGGER[W] = {
    listGen.asOptionMsgs(msgs, value)
  }

  def doAsEither[W: TypeTag](value: Either[Message,W]): LOGGER[W] = {
    listGen.asEither(value)
  }
  def doAsEitherMsg[W: TypeTag](msg: String, value: Either[Message,W]): LOGGER[W] = {
    listGen.asEitherMsg(msg, value)
  }
  def doAsEitherMsgs[W: TypeTag](msgs: List[String], value: Either[Message,W]): LOGGER[W] = {
    listGen.asEitherMsgs(msgs, value)
  }

  def doAsTry[W: TypeTag](value: Try[W]): LOGGER[W] = {
    listGen.asTry(value)
  }
  def doAsTryMsg[W: TypeTag](msg: String, value: Try[W]): LOGGER[W] = {
    listGen.asTryMsg(msg, value)
  }
  def doAsTryMsgs[W: TypeTag](msgs: List[String], value: Try[W]): LOGGER[W] = {
    listGen.asTryMsgs(msgs, value)
  }


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

    doAsMsg("Msg 1", 5).get should be (Value(5))
    doAsMsg("Msg 1", 5).getRaw should be (5)
    doAsMsg("Msg 1", 5).getValue should be (Some(5))
    doAsMsg("Msg 1", 5).log should be (List("Msg 1"))
    doAsMsg("Msg 1", "Test 2").get should be (Value("Test 2"))
    doAsMsg("Msg 1", "Test 2").getRaw should be ("Test 2")
    doAsMsg("Msg 1", "Test 2").getValue should be (Some("Test 2"))
    doAsMsg("Msg 1", "Test 2").log should be (List("Msg 1"))

    doAsMsgs(List("Msg 1"), 6).get should be (Value(6))
    doAsMsgs(List("Msg 1"), 6).getRaw should be (6)
    doAsMsgs(List("Msg 1"), 6).getValue should be (Some(6))
    doAsMsgs(List("Msg 1"), 6).log should be (List("Msg 1"))
    doAsMsgs(List("Msg 1"), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List("Msg 1"), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List("Msg 1"), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List("Msg 1"), "Test 3").log should be (List("Msg 1"))

    doAsMsgs(List("Msg 1","Msg 2"), 6).get should be (Value(6))
    doAsMsgs(List("Msg 1","Msg 2"), 6).getRaw should be (6)
    doAsMsgs(List("Msg 1","Msg 2"), 6).getValue should be (Some(6))
    doAsMsgs(List("Msg 1","Msg 2"), 6).log should be (List("Msg 1","Msg 2"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").get should be (Value("Test 3"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").getRaw should be ("Test 3")
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").getValue should be (Some("Test 3"))
    doAsMsgs(List("Msg 1","Msg 2"), "Test 3").log should be (List("Msg 1","Msg 2"))
  }

  it should "Log error values equal" in {
    doAs(4).hasError should be (false)
    doAs("Test 3").hasError should be (false)

    doAsMsg("Msg 1", 5).hasError should be (false)
    doAsMsg("Msg 1", "Test 3").hasError should be (false)

    doAsMsgs(List("Msg 1"), 5).hasError should be (false)
    doAsMsgs(List("Msg 1"), "Test 3").hasError should be (false)

    doAsMsgs(List("Msg 1", "Msg 2"), 5).hasError should be (false)
    doAsMsgs(List("Msg 1", "Msg 2"), "Test 3").hasError should be (false)
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

    doAsOptionMsg("Msg 2", None).get should be (OptionError)
    doAsOptionMsg("Msg 2", None).getValue should be (None)
    doAsOptionMsg("Msg 2", None).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(4)).get should be (Value(4))
    doAsOptionMsg("Msg 2", Some(4)).getValue should be (Some(4))
    doAsOptionMsg("Msg 2", Some(4)).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsg("Msg 2", Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsg("Msg 2", Some("Test 1")).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsg("Msg 2", Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsg("Msg 2", Some(List(4))).log should be (List("Msg 2"))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsg("Msg 2", Some(List("Test 1"))).log should be (List("Msg 2"))

    doAsOptionMsgs(List("Msg 2"), None).get should be (OptionError)
    doAsOptionMsgs(List("Msg 2"), None).getValue should be (None)
    doAsOptionMsgs(List("Msg 2"), None).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List("Msg 2"), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List("Msg 2"), Some(4)).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List("Msg 2"), Some("Test 1")).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List("Msg 2"), Some(List(4))).log should be (List("Msg 2"))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List("Msg 2"), Some(List("Test 1"))).log should be (List("Msg 2"))

    doAsOptionMsgs(List("Msg 2","Msg 3"), None).get should be (OptionError)
    doAsOptionMsgs(List("Msg 2","Msg 3"), None).getValue should be (None)
    doAsOptionMsgs(List("Msg 2","Msg 3"), None).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).get should be (Value(4))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).getValue should be (Some(4))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(4)).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).get should be (Value("Test 1"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).getValue should be (Some("Test 1"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some("Test 1")).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).get should be (Value(List(4)))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).getValue should be (Some(List(4)))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List(4))).log should be (List("Msg 2","Msg 3"))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).get should be (Value(List("Test 1")))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).getValue should be (Some(List("Test 1")))
    doAsOptionMsgs(List("Msg 2","Msg 3"), Some(List("Test 1"))).log should be (List("Msg 2","Msg 3"))
  }
  it should "Log Option error values equal" in {
    doAsOption(None).hasError should be (true)
    doAsOption(None).hasValue should be (false)
    doAsOption(Some(4)).hasError should be (false)
    doAsOption(Some("hi")).hasError should be (false)

    doAsOptionMsg("Msg 3", None).hasError should be (true)
    doAsOptionMsg("Msg 3", None).hasValue should be (false)
    doAsOptionMsg("Msg 3", Some(4)).hasError should be (false)
    doAsOptionMsg("Msg 3", Some("hi")).hasError should be (false)

    doAsOptionMsgs(List("Msg 3"), None).hasError should be (true)
    doAsOptionMsgs(List("Msg 3"), None).hasValue should be (false)
    doAsOptionMsgs(List("Msg 3"), Some(4)).hasError should be (false)
    doAsOptionMsgs(List("Msg 3"), Some("hi")).hasError should be (false)
  }
  it should "Log Either values equal" in {
    doAsEither(Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEither(Left(Message("A problem"))).getValue should be (None)
    doAsEither(Right(4)).getValue should be (Some(4))
    doAsEither(Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEither(Right(List(4))).getValue should be (Some(List(4)))
    doAsEither(Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsg("Msg 6", Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEitherMsg("Msg 6", Left(Message("A problem"))).getValue should be (None)
    doAsEitherMsg("Msg 6", Right(4)).getValue should be (Some(4))
    doAsEitherMsg("Msg 6", Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsg("Msg 6", Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsg("Msg 6", Right(List("Test 1"))).getValue should be (Some(List("Test 1")))

    doAsEitherMsgs(List("Msg 6"), Left(Message("A problem"))).get should be (EitherError(Message("A problem")))
    doAsEitherMsgs(List("Msg 6"), Left(Message("A problem"))).getValue should be (None)
    doAsEitherMsgs(List("Msg 6"), Right(4)).getValue should be (Some(4))
    doAsEitherMsgs(List("Msg 6"), Right("Test 1")).getValue should be (Some("Test 1"))
    doAsEitherMsgs(List("Msg 6"), Right(List(4))).getValue should be (Some(List(4)))
    doAsEitherMsgs(List("Msg 6"), Right(List("Test 1"))).getValue should be (Some(List("Test 1")))
  }
  it should "Log Either error values equal" in {
    doAsEither(Left(Message("Got Error"))).hasError should be (true)
    doAsEither(Left(Message("Got Error"))).hasValue should be (false)
    doAsEither(Right(4)).hasError should be (false)
    doAsEither(Right(4)).hasValue should be (true)
    doAsEither(Right("hi")).hasError should be (false)
    doAsEither(Right("hi")).hasValue should be (true)

    doAsEitherMsg("Msg 3", Left(Message("Got Error"))).hasError should be (true)
    doAsEitherMsg("Msg 3", Left(Message("Got Error"))).hasValue should be (false)
    doAsEitherMsg("Msg 3", Right(4)).hasError should be (false)
    doAsEitherMsg("Msg 3", Right("hi")).hasError should be (false)

    doAsEitherMsgs(List("Msg 3"), Left(Message("Got Error"))).hasError should be (true)
    doAsEitherMsgs(List("Msg 3"), Left(Message("Got Error"))).hasValue should be (false)
    doAsEitherMsgs(List("Msg 3"), Right(4)).hasError should be (false)
    doAsEitherMsgs(List("Msg 3"), Right("hi")).hasError should be (false)
  }

  it should "Log try values equal" in {
    doAsTry(Success(4)).get should be (Value(4))
    doAsTry(Success(4)).getRaw should be (Success(4))
    doAsTry(Success(4)).getValue should be (Some(4))
    doAsTry(Success(4)).getError should be (None)
    doAsTry(Success(4)).hasError should be (false)
    doAsTry(Success(4)).hasValue should be (true)

    doAsTryMsg("Msg 9", Success("hi there")).get should be (Value("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsg("Msg 9", Success("hi there")).getError should be (None)
    doAsTryMsg("Msg 9", Success("hi there")).hasError should be (false)
    doAsTryMsg("Msg 9", Success("hi there")).hasValue should be (true)

    doAsTryMsgs(List("Msg 9"), Success("hi there")).get should be (Value("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getRaw should be (Success("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getValue should be (Some("hi there"))
    doAsTryMsgs(List("Msg 9"), Success("hi there")).getError should be (None)
    doAsTryMsgs(List("Msg 9"), Success("hi there")).hasError should be (false)
    doAsTryMsgs(List("Msg 9"), Success("hi there")).hasValue should be (true)
  }

  it should "Log error try values equal" in {
    val ex = new Exception("hi")
    doAsTry(Failure(ex)).get should be (TryError(ex))
    doAsTry(Failure(ex)).getRaw should be (Failure(ex))
    doAsTry(Failure(ex)).getValue should be (None)
    doAsTry(Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTry(Failure(ex)).hasValue should be (false)
    doAsTry(Failure(ex)).hasError should be (true)

    doAsTryMsg("Msg 9",Failure(ex)).get should be (TryError(ex))
    doAsTryMsg("Msg 9",Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsg("Msg 9",Failure(ex)).getValue should be (None)
    doAsTryMsg("Msg 9",Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsg("Msg 9",Failure(ex)).hasValue should be (false)
    doAsTryMsg("Msg 9",Failure(ex)).hasError should be (true)

    doAsTryMsgs(List("Msg 9"),Failure(ex)).get should be (TryError(ex))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getRaw should be (Failure(ex))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getValue should be (None)
    doAsTryMsgs(List("Msg 9"),Failure(ex)).getError should be (Some(TryError(ex)))
    doAsTryMsgs(List("Msg 9"),Failure(ex)).hasValue should be (false)
    doAsTryMsgs(List("Msg 9"),Failure(ex)).hasError should be (true)
  }
}


/**
 * Tests function calls
 */
class FunctionStringListSpec extends FlatSpec with Matchers {

  import Generator.StringAsList._

  type LOGGER[W] = Logger[String,List[String],String,W]

  val listGen = new Generator.AsList[String]

  def doAs[W: TypeTag](value: W): LOGGER[W] = {
    listGen.as(value)
  }
  def doAsEither[W: TypeTag](value: Either[String,W]): LOGGER[W] = {
    listGen.asEither(value)
  }

  def doUnit(n: Int): LOGGER[Unit] = {}

  def addOne(n: Int): LOGGER[Int] =
    s"addOne: to $n" ~> (n + 1)

  def adding(n: Int, v: Int): LOGGER[Int] =
    (s"adding: $n to $v") ~> (n + v)

  def doOption(n: Int): LOGGER[Int] = {
    asOptionMsg(s"doOption: $n minus 4", Some(n-4))
  }
  def doOptionA(n: Int): LOGGER[Int] = {
    s"doOptionA: $n minus 4" ~> Some(n-4)
  }

  def doOptionError(n: Int): LOGGER[Int] = {
    asOptionMsg(s"doOptionError: $n minus 4", None)
  }
  def doOptionErrorA(n: Int): LOGGER[Int] = {
    s"doOptionErrorA: $n minus 4" ~> None
  }

  def doEither(n: Int): LOGGER[Int] = {
    asEitherMsg(s"doEither: $n plus 4", Right(n+4))
  }
  def doEitherA(n: Int): LOGGER[Int] = {
    s"doEitherA: $n plus 4" ~> Right(n+4)
  }

  def doEitherError(n: Int): LOGGER[Int] = {
    asEitherMsg(s"doEitherError: $n plus 4", Left("Got Either Error"))
  }
  def doEitherErrorA(n: Int): LOGGER[Int] = {
    s"doEitherErrorA: $n plus 4" ~> Left("Got Either Error")
  }

  def doTry(n: Int): LOGGER[Int] = {
    asTryMsg(s"doTry: $n times 2", Success(n*2))
  }

  def doTryA(n: Int): LOGGER[Int] = {
    s"doTryA: $n times 2" ~> Success(n*2)
  }

  val ex = new Exception("Got Try Error")
  def doTryError(n: Int): LOGGER[Int] = {
    asTryMsg(s"doTryError: $n times 2", Failure(ex))
  }
  def doTryErrorA(n: Int): LOGGER[Int] = {
    s"doTryErrorA: $n times 2" ~> Failure(ex)
  }

  def doEnterLeave(n: Int, v: Int): LOGGER[Int] = enter_leave {
    (s"doEnterLeave: $n plus $v") ~> (n + v)
  }
  def doEnterLeaveOptionError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asOptionMsg(s"doEnterLeaveOptionError: $n minus $v", None)
  }
  def doEnterLeaveEitherError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asEitherMsg(s"doEnterLeaveEitherError: $n minus $v", Left("Got Either Error"))
  }
  def doEnterLeaveTryError(n: Int, v: Int): LOGGER[Int] = enter_leave {
    asTryMsg(s"doEnterLeaveTryError: $n minus $v", Failure(ex))
  }

  def doBlockPlusFourValue(n: Int): LOGGER[Int] = block { log =>
    log(n+4)
  }
  def doBlockPlusFourOptionValue(n: Int): LOGGER[Int] = block { log =>
    log(Some(n+4))
  }
  def doBlockPlusFourOptionError(n: Int): LOGGER[Int] = block { log =>
    log(None)
  }
  def doBlockPlusFourEitherValue(n: Int): LOGGER[Int] = block { log =>
    log(Right(n+4))
  }
  def doBlockPlusFourEitherError(n: Int): LOGGER[Int] = block { log =>
    log(Left("Bad"))
  }
  def doBlockPlusFourTryValue(n: Int): LOGGER[Int] = block { log =>
    log(Success(n+4))
  }
  def doBlockPlusFourTryError(n: Int): LOGGER[Int] = block { log =>
    log(Failure(ex))
  }

  def doBlock(n: Int): LOGGER[Int] = block { log =>
    log("doBlock Top")
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
      log.prepend("doBlock Bottom")
    }
  }

  def doBlockOption1Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockOption1Error Top")
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
      log.prepend("doBlockOption1Error Bottom")
    }
  }

  def doBlockOption2Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockOption2Error Top")
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
      log.prepend("doBlockOption2Error Bottom")
    }
  }

  def doBlockEither1Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockEither1Error Top")
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
      log.prepend("doBlockEither1Error Bottom")
    }
  }

  def doBlockEither2Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockEither2Error Top")
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
      log.prepend("doBlockEither2Error Bottom")
    }
  }

  def doBlockTry1Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockTry1Error Top")
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
      log.prepend("doBlockTry1Error Bottom")
    }
  }

  def doBlockTry2Error(n: Int): LOGGER[Int] = block { log =>
    log("doBlockTry2Error Top")
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
      log.prepend("doBlockTry2Error Bottom")
    }
  }
  
  def doForEither(n: Int): LOGGER[Int] = { 
    for(
      a <- adding(n, 4);
      b <- adding(a, 5);
      c = b+3
     ) yield c
  }

  def doNoErrors(n: Int): LOGGER[Unit] = { 
    for(
      a <- adding(n, 4);
      b <- adding(a, 5);
      c <- doUnit(b)
     ) yield c
  }
  def doString(): LOGGER[String] = { 
    doAs("Some String")
  }
  def doStringError(): LOGGER[String] = { 
    doAsEither(Left("No String"))
  }
  // XXXXX
  def doConcat(n: Int): LOGGER[Int] = { 
    val l1 =  adding(n, 1);
    l1.getValue match {
      case Some(v) => adding(v, 2).concat(l1)
      case None => l1
    }
  }

  def doOp(n1: Int, n2: Int): LOGGER[Int] = { 
    val l1 = adding(n1, 1)
    val l2 = adding(n2, 2)
    (l1 op l2) { case (a,b) => a + b }

  }

  def doList(numbers: List[Int]): LOGGER[Int] = { 
    val l: LOGGER[Int] = doAs(0)
    for(
      a <- numbers.foldLeft(l){(result,n) => 
            (adding(n, 1) op result) { case (a,b) => a + b }
          };
      b <- adding(a, 100)
     ) yield b
  }

  def doStringLengthSimple(
    str: String
  ): LOGGER[Int] = {
    asOptionMsg(s"TRACE: str=$str", Some(str.length))
  }

  def getStringLengthSimple(
    str: String
  ): LOGGER[Int] = {
    for (
      l <- doStringLengthSimple(str)
    ) yield l
  }

  def doStringLengthTuple(
    str: String
  ): LOGGER[Tuple2[String,Int]] = {
    asOptionMsg(s"TRACE: str=$str", Some(Tuple2(str, str.length)))
  }

  def getStringLengthTuple(
    str: String
  ): LOGGER[Int] = {
// NOTE: TUPLE matching not working  with Logger.getLoggerValue implict
/*
    for (
      (s,l) <- doStringLengthTuple(str)
    ) yield l
    for (
      x <- doStringLengthTuple(str)
    ) yield x._2
*/
    for (
      t <- doStringLengthTuple(str);
      (s,l) <- t
    ) yield l
  }


  it should "Return value and log entry" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.get should be (Value(11))
    l1.log should be (List("addOne: to 10"))

    val l2 = for (a <- adding(in, 5)) yield a
    l2.get should be (Value(15))
    l2.log should be (List("adding: 10 to 5"))

    val l3 = for (a <- doOption(in)) yield a
    l3.get should be (Value(6))
    l3.log should be (List("doOption: 10 minus 4"))

    val l3A = for (a <- doOptionA(in)) yield a
    l3A.get should be (Value(6))
    l3A.log should be (List("doOptionA: 10 minus 4"))

    val l4 = for (a <- doEither(in)) yield a
    l4.get should be (Value(14))
    l4.log should be (List("doEither: 10 plus 4"))

    val l4A = for (a <- doEitherA(in)) yield a
    l4A.get should be (Value(14))
    l4A.log should be (List("doEitherA: 10 plus 4"))

    val l5 = for (a <- doTry(in)) yield a
    l5.get should be (Value(20))
    l5.log should be (List("doTry: 10 times 2"))

    val l5A = for (a <- doTryA(in)) yield a
    l5A.get should be (Value(20))
    l5A.log should be (List("doTryA: 10 times 2"))

    val l6 = for (a <- doEnterLeave(in, 12)) yield a
    l6.get should be (Value(22))
    l6.log should be (List("ENTER", "doEnterLeave: 10 plus 12", "LEAVE"))

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield b
    l7.get should be (Value(16))
    l7.log should be (List("adding: 10 to 5", "addOne: to 15"))

    val l7A = 
      for (
        a <- adding(in, 5);
        b <- addOne(a)
      ) yield (a,b)
    l7A.get should be (Value(15,16))
    l7A.log should be (List("adding: 10 to 5", "addOne: to 15"))

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- addOne(a);
        c <- doOption(b);
        d <- doEither(c);
        e <- doTry(d)
      ) yield e
    l8.get should be (Value(32))
    l8.log should be (List("adding: 10 to 5", "addOne: to 15", "doOption: 16 minus 4", "doEither: 12 plus 4", "doTry: 16 times 2"))

    val l9 = for (a <- doBlock(in)) yield a
    l9.get should be (Value(43))
    l9.log should be (List("doBlock Top", "adding: 10 to 4", "adding: 14 to 5", "adding: 19 to 22", "adding: 41 to 2", "doBlock Bottom"))
  }
  it should "Log error detected" in {
    val in = 10

    val l1 = for (a <- addOne(in)) yield a
    l1.hasError should be (false)

    val l2 = for (a <- adding(in, 5)) yield a
    l2.hasError should be (false)

    val l3 = for (a <- doOption(in)) yield a
    l3.hasError should be (false)

    val l3A = for (a <- doOptionA(in)) yield a
    l3A.hasError should be (false)

    val l4 = for (a <- doEither(in)) yield a
    l4.hasError should be (false)

    val l4A = for (a <- doEitherA(in)) yield a
    l4A.hasError should be (false)

    val l5 = for (a <- doTry(in)) yield a
    l5.hasError should be (false)

    val l5A = for (a <- doTryA(in)) yield a
    l5A.hasError should be (false)

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
    l1.log should be (List("doOptionError: 10 minus 4"))

    val l1A = for (a <- doOptionErrorA(in)) yield a
    l1A.hasError should be (true)
    l1A.get should be (OptionError)
    l1A.log should be (List("doOptionErrorA: 10 minus 4"))

    val l2 = for (a <- doEitherError(in)) yield a
    l2.hasError should be (true)
    l2.get should be (EitherError("Got Either Error"))
    l2.log should be (List("doEitherError: 10 plus 4"))

    val l2A = for (a <- doEitherErrorA(in)) yield a
    l2A.hasError should be (true)
    l2A.get should be (EitherError("Got Either Error"))
    l2A.log should be (List("doEitherErrorA: 10 plus 4"))

    val l3 = for (a <- doTryError(in)) yield a
    l3.hasError should be (true)
    l3.get should be (TryError(ex))
    l3.log should be (List("doTryError: 10 times 2"))

    val l3A = for (a <- doTryErrorA(in)) yield a
    l3A.hasError should be (true)
    l3A.get should be (TryError(ex))
    l3A.log should be (List("doTryErrorA: 10 times 2"))

    val l4 = 
      for (
        a <- adding(in, 5);
        b <- doOptionError(a)
      ) yield b
    l4.hasError should be (true)
    l4.get should be (OptionError)
    l4.log should be (List("adding: 10 to 5", "doOptionError: 15 minus 4"))

    val l5 = 
      for (
        a <- adding(in, 5);
        b <- doOptionError(a);
        c <- addOne(b)
      ) yield c
    l5.hasError should be (true)
    l5.get should be (OptionError)
    l5.log should be (List("adding: 10 to 5", "doOptionError: 15 minus 4"))

    val l6 = 
      for (
        a <- adding(in, 5);
        b <- doEitherError(a)
      ) yield b
    l6.hasError should be (true)
    l6.get should be (EitherError("Got Either Error"))
    l6.log should be (List("adding: 10 to 5", "doEitherError: 15 plus 4"))

    val l7 = 
      for (
        a <- adding(in, 5);
        b <- doEitherError(a);
        c <- addOne(b)
      ) yield c
    l7.hasError should be (true)
    l7.get should be (EitherError("Got Either Error"))
    l7.log should be (List("adding: 10 to 5", "doEitherError: 15 plus 4"))

    val l8 = 
      for (
        a <- adding(in, 5);
        b <- doTryError(a)
      ) yield b
    l8.hasError should be (true)
    l8.get should be (TryError(ex))
    l8.log should be (List("adding: 10 to 5", "doTryError: 15 times 2"))

    val l9 = 
      for (
        a <- adding(in, 5);
        b <- doTryError(a);
        c <- addOne(b)
      ) yield c
    l9.hasError should be (true)
    l9.get should be (TryError(ex))
    l9.log should be (List("adding: 10 to 5", "doTryError: 15 times 2"))

    val l10 = for (a <- doEnterLeaveOptionError(in, 1)) yield a
    l10.hasError should be (true)
    l10.get should be (OptionError)
    l10.log should be (List("ENTER", "doEnterLeaveOptionError: 10 minus 1", "LEAVE"))

    val l11 = for (a <- doEnterLeaveEitherError(in, 1)) yield a
    l11.hasError should be (true)
    l11.get should be (EitherError("Got Either Error"))
    l11.log should be (List("ENTER", "doEnterLeaveEitherError: 10 minus 1", "LEAVE"))

    val l12 = for (a <- doEnterLeaveTryError(in, 1)) yield a
    l12.hasError should be (true)
    l12.get should be (TryError(ex))
    l12.log should be (List("ENTER", "doEnterLeaveTryError: 10 minus 1", "LEAVE"))

    val l13 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveOptionError(a, 4);
        c <- addOne(b)
      ) yield c
    l13.hasError should be (true)
    l13.get should be (OptionError)
    l13.log should be (List("adding: 10 to 5", "ENTER", "doEnterLeaveOptionError: 15 minus 4", "LEAVE"))

    val l14 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveEitherError(a, 4);
        c <- addOne(b)
      ) yield c
    l14.hasError should be (true)
    l14.get should be (EitherError("Got Either Error"))
    l14.log should be (List("adding: 10 to 5", "ENTER", "doEnterLeaveEitherError: 15 minus 4", "LEAVE"))

    val l15 = 
      for (
        a <- adding(in, 5);
        b <- doEnterLeaveTryError(a, 4);
        c <- addOne(b)
      ) yield c
    l15.hasError should be (true)
    l15.get should be (TryError(ex))
    l15.log should be (List("adding: 10 to 5", "ENTER", "doEnterLeaveTryError: 15 minus 4", "LEAVE"))

    val l16 = for (a <- doBlockOption1Error(in)) yield a
    l16.hasError should be (true)
    l16.get should be (OptionError)
    l16.log should be (List("doBlockOption1Error Top", "adding: 10 to 4", "doOptionError: 14 minus 4", "doBlockOption1Error Bottom"))

    val l17 = for (a <- doBlockOption2Error(in)) yield a
    l17.hasError should be (true)
    l17.get should be (OptionError)
    l17.log should be (List("doBlockOption2Error Top", "adding: 10 to 4", "adding: 14 to 5", "adding: 19 to 22", "doOptionError: 41 minus 4", "doBlockOption2Error Bottom"))

    val l18 = for (a <- doBlockEither1Error(in)) yield a
    l18.hasError should be (true)
    l18.get should be (EitherError("Got Either Error"))
    l18.log should be (List("doBlockEither1Error Top", "adding: 10 to 4", "doEitherError: 14 plus 4", "doBlockEither1Error Bottom"))

    val l19 = for (a <- doBlockEither2Error(in)) yield a
    l19.hasError should be (true)
    l19.get should be (EitherError("Got Either Error"))
    l19.log should be (List("doBlockEither2Error Top", "adding: 10 to 4", "adding: 14 to 5", "adding: 19 to 22", "doEitherError: 41 plus 4", "doBlockEither2Error Bottom"))

    val l20 = for (a <- doBlockTry1Error(in)) yield a
    l20.hasError should be (true)
    l20.get should be (TryError(ex))
    l20.log should be (List("doBlockTry1Error Top", "adding: 10 to 4", "doTryError: 14 times 2", "doBlockTry1Error Bottom"))

    val l21 = for (a <- doBlockTry2Error(in)) yield a
    l21.hasError should be (true)
    l21.get should be (TryError(ex))
    l21.log should be (List("doBlockTry2Error Top", "adding: 10 to 4", "adding: 14 to 5", "adding: 19 to 22", "doTryError: 41 times 2", "doBlockTry2Error Bottom"))

    val l22 = for (a <- doBlockPlusFourValue(in)) yield a
    l22.hasError should be (false)
    l22.get should be (Value(14))

    val l23 = for (a <- doBlockPlusFourOptionValue(in)) yield a
    l23.hasError should be (false)
    l23.get should be (Value(14))

    val l24 = for (a <- doBlockPlusFourOptionError(in)) yield a
    l24.hasError should be (true)
    l24.get should be (OptionError)

    val l25 = for (a <- doBlockPlusFourEitherValue(in)) yield a
    l25.hasError should be (false)
    l25.get should be (Value(14))

    val l26 = for (a <- doBlockPlusFourEitherError(in)) yield a
    l26.hasError should be (true)
    l26.get should be (EitherError("Bad"))

    val l27 = for (a <- doBlockPlusFourTryValue(in)) yield a
    l27.hasError should be (false)
    l27.get should be (Value(14))

    val l28 = for (a <- doBlockPlusFourTryError(in)) yield a
    l28.hasError should be (true)
    l28.get should be (TryError(ex))

    val l29 = for (a <- doForEither(in)) yield a
    l29.hasError should be (false)
    l29.get should be (Value(22))

    val l30 = for (a <- doNoErrors(in)) yield a
    l30.hasError should be (false)
    l30.get should be (Value(()))

    val l31 = doString()
    val sOp31 =l31.getValue
    sOp31 should be (Some("Some String"))

    val l32 = doStringError()
    val sOp32 =l32.getValue
    sOp32 should be (None)
  }

  it should "special cases work" in {
    val in = 10

  // XXXXX
    val l1 = doConcat(in)
    l1.hasError should be (false)
    l1.get should be (Value(13))
    l1.log should be (List("adding: 11 to 2", "adding: 10 to 1"))

    val l2 = doOp(1, 10)
    l2.hasError should be (false)
    l2.get should be (Value(14))
    l2.log should be (List("adding: 10 to 2", "adding: 1 to 1"))

    val l3 = doList(List(1,2,3))
    l3.hasError should be (false)
    l3.get should be (Value(109))
    l3.log should be (List("adding: 1 to 1", "adding: 2 to 1", "adding: 3 to 1", "adding: 9 to 100"))

    val l4 = getStringLengthSimple("1234")
    l4.hasError should be (false)
    l4.get should be (Value(4))

    val l5 = getStringLengthTuple("12345")
    l5.hasError should be (false)
    l5.get should be (Value(5))

    val l6 = {
        val r = for (
                  a <- adding(in, 5);
                  c <- addOne(a)
                ) yield c
        "INFO: start" ~> r
      }
    l6.hasError should be (false)
    l6.get should be (Value(16))
    l6.log should be (List("INFO: start", "adding: 10 to 5", "addOne: to 15"))

    val l7 = {
        val r = for (
                  a <- adding(in, 5);
                  c <- addOne(a)
                ) yield c
        r ~> "INFO: return"
      }
    l7.hasError should be (false)
    l7.get should be (Value(16))
    l7.log should be (List("adding: 10 to 5", "addOne: to 15","INFO: return"))

    val l8 = {
        val r = for (
                  a <- adding(in, 5);
                  c <- addOne(a)
                ) yield c
        "INFO: start" ~> r ~> "INFO: return"
      }
    l8.hasError should be (false)
    l8.get should be (Value(16))
    l8.log should be (List("INFO: start", "adding: 10 to 5", "addOne: to 15","INFO: return"))

    val l9 = {
        val r = for (
                  a <- adding(in, 5);
                  b <- doEitherError(a);
                  c <- addOne(b)
                ) yield c
        "INFO: start" ~> r
      }
    l9.hasError should be (true)
    l9.get should be (EitherError("Got Either Error"))
    l9.log should be (List("INFO: start", "adding: 10 to 5", "doEitherError: 15 plus 4"))

    val l10 = {
        "INFO: Start" ~> {
          for (
            a <- adding(in, 5);
            b <- doEitherError(a);
            c <- addOne(b)
          ) yield c
        } ~> "INFO: return"
      }
    l10.hasError should be (true)
    l10.get should be (EitherError("Got Either Error"))
    l10.log should be (List("INFO: Start", "adding: 10 to 5", "doEitherError: 15 plus 4", "INFO: return"))

    val l11 = {
            var cnt: Int = 0
            def doAdd(x: Int): LOGGER[Unit] = {
              (cnt += x)
              s"doAdd $x" ~> 
            }
            val r = for (
             n <- List(1,2,3,4);
             b = doAdd(n)
            ) yield b

            // println(s"r=$r")
            // println(s"cnt=$cnt")
            r ~> ("INFO: return" ~> cnt)
          }
    l11.hasError should be (false)
    l11.get should be (Value(10))
    l11.log should be (List("doAdd 1", "doAdd 2", "doAdd 3", "doAdd 4", "INFO: return"))

    val l12 = {
            val l = adding(in, 5)
            l.replace(44)
          }
    l12.hasError should be (false)
    l12.get should be (Value(44))
    l12.log should be (List("adding: 10 to 5"))

    val l13 = {
            val l = adding(in, 5)
            l.replace("Hi")
          }
    l13.hasError should be (false)
    l13.get should be (Value("Hi"))
    l13.log should be (List("adding: 10 to 5"))

    val l14 = {
            def getTuple: LOGGER[Tuple3[Int, String, List[Int]]] = {
              "getTuple" ~> (44, "Hi", List(1,2,3))
            }
            def getBD: LOGGER[BigDecimal] = BigDecimal(22)
            def doString(s: String): LOGGER[String] = {
              s"doString $s" ~> (s + s)
            }
            val l = for (
              t <- getTuple;
              (n, s, l) <- t;
              bd <- getBD;
              ss <- doString(s)
            ) yield (n,ss)
            l
          }
    l14.hasError should be (false)
    l14.get should be (Value((44, "HiHi")))
    l14.log should be (List("getTuple", "doString Hi"))

/*
// NOTE: TUPLE matching not working  with Logger.getLoggerValue implict
{
  val tuple = "getTuple" ~> (44, "Hi", List(1,2,3))
  val l = for (
    (n, s, l) <- tuple
  ) yield (n, s)
  println(s"l=${l}")
  println(s"l.get=${l.get}")
  val ll = for (
    t <- tuple
  ) yield (t._1, t._2)
  println(s"ll=${ll}")
  println(s"ll.get=${ll.get}")
  val lv = for (
    t <- tuple;
    (n, s, l) <- t
  ) yield (n, s)
  println(s"lv=${lv}")
  println(s"lv.get=${lv.get}")
}
*/

    val l15 = {
            def got(n: Int): Logger[String,List[String],Long,Int] = 
                s"Got $n" ~> n

            val l1 = got(2)
            val l2 = got(3)
            l1.concat(l2)
          }
    l15.hasError should be (false)
    l15.get should be (Value(2))
    l15.log should be (List("Got 2", "Got 3"))

    val l16 = {
            val l1 = "Got 2" ~> 2
            val l2 = "Got 3" ~> 3
            l1.concat(l2)
          }
    l16.hasError should be (false)
    l16.get should be (Value(2))
    l16.log should be (List("Got 2", "Got 3"))

    val l17 = {
            val l1: Logger[String,List[String],Long,List[Int]] = "Got 2" ~> List(1,2)
            val l2: Logger[String,List[String],Long,List[Int]] = "Got 3" ~> List(3,4,5)
            l1 ++ l2
          }
    l17.hasError should be (false)
    l17.get should be (Value(List(1,2,3,4,5)))
    l17.log should be (List("Got 2", "Got 3"))

    val l18 = {
            val l = List(1,2)
            val logger: Logger[String,List[String],Long,List[Int]] = "Got 3" ~> List(3,4,5)
            l dojoin logger
          }
    l18.hasError should be (false)
    l18.get should be (Value(List(1,2,3,4,5)))
    l18.log should be (List("Got 3"))

    val l19 = {
            val l1 = List(1,2)
            val l2 = List(3)
            val logger: Logger[String,List[String],Long,List[Int]] = "Got 3" ~> List(4,5)
            l1 ++ l2 dojoin logger
          }
    l19.hasError should be (false)
    l19.get should be (Value(List(1,2,3,4,5)))
    l19.log should be (List("Got 3"))

    val l20 = {
            val l: List[Logger[String,List[String],Long,Int]] = List("Got 1" ~> 1, "Got 2" ~> 2)
            l.doflatten
          }
    l20.hasError should be (false)
    l20.get should be (Value(List(1,2)))
    l20.log should be (List("Got 1", "Got 2"))

    val l21 = {
            val l: List[Logger[String,List[String],Long,List[Int]]] = 
                List("Got 1" ~> List(1,2), "Got 2" ~> List(3,4,5))
            l.doflatten
          }
    l21.hasError should be (false)
    l21.get should be (Value(List(1,2,3,4,5)))
    l21.log should be (List("Got 1", "Got 2"))

    val v1 = {
            val l = "Make true" ~> true
            mapValue(l){w=> "Got True"}
          }
    v1 should be (Some("Got True"))

    // tests lifting value into a Logger
    def get44(b: Boolean) : LOGGER[Int] = {
      if (b) 44
      else "Could not get 44" ~> None
    }

    val l22 = get44(true)
    l22.hasError should be (false)
    l22.get should be (Value(44))
    l22.log should be (List())

    val l23 = get44(false)
    l23.hasError should be (true)
    l23.get should be (OptionError)
    l23.log should be (List("Could not get 44"))

/*
    val (l24, v24) = {
            var v: Int = 0

            def doubleInt(n: Int): LOGGER[Int] = {
              s"INFO: Doubling $n" ~> (n+n)
            }
            def processList(l: List[Int]): LOGGER[Unit] = block {log =>
              log("TRACE: processList top")

              l.foreach{ n=>
                v += log("did it" ~> doubleInt(n))
              }
              log("TRACE: processList bottom")
            }

            val list = List(1,2,3)
            val l = processList(list)
            (l, v)
          }
    l24.hasError should be (true)
    l24.get should be (Value(()))
    v24.get should be (12)
    l24.log should be (List("Could not get 44"))
*/
  }

/*
  it should "block LogCient work" in {

    def doBlock1(n: Int): LOGGER[Int] = blockNew { log =>
      log("Start")
      // val i: Int = log(n+4)
      val i = log(n+4)
      log("End", i)
    }
    def doBlock2(n: Int): LOGGER[Int] = blockNew { log =>
      log("Start")
      val i: Int = log(adding(1,n)).getOrElse(44)
      log("End", i)
    }
    def doBlockError(n: Int): LOGGER[Int] = blockNew { log =>
      log("Start")
      log(doOptionError(n)) match {
        case Some(i) => log("value", i)
        case None => 
      }
      log("End")
    }

    val l1 = doBlock1(4)
    l1.hasError should be (false)
    l1.get should be (Value(8))
    l1.log should be (List("End", "Start"))

    val l2 = doBlock2(4)
    l2.hasError should be (false)
    l2.get should be (Value(5))
    l2.log should be (List("End", "adding: 1 to 4", "Start"))

    val l3 = doBlockError(4)
    l3.hasError should be (true)
    l3.get should be (Value(5))
    l3.log should be (List("End", "adding: 1 to 4", "Start"))



  }
  it should "block LogCient work" in {
    def doBlock1(n: Int): LOGGER[Int] = blockX { log =>
      log("Start")
      // val i: Int = log(n+4)
      val i = log(n+4)
      log("End", i)
    }
  }
*/
}
