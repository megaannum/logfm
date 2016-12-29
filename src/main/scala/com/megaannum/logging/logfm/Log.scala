package com.megaannum.logging.logfm

object Log {
  // log levels (based upon Apache levels):
  // emerg:  System is not usable
  // alert:  Severe situation prompt action needed
  // crit:   Important problems need to be addressed
  // error:  Something was not successful
  // warn:   Out of ordinary but not a cause for concern
  // notice: Normal but worth noting
  // info:   Nice to know
  // debug:  Useful for pinpointing problem area
  // trace:  Large amounts for inforation
  object Level extends Enumeration {
    type Type = Value

    val TRACE = Value("TRACE")
    val DEBUG = Value("DEBUG")
    val INFO = Value("INFO")
    val NOTICE = Value("NOTICE")
    val WARN = Value("WARN")
    val ERROR = Value("ERROR")
    val CRIT = Value("CRIT")
    val ALERT = Value("ALERT")
    val EMERG = Value("EMERG")
  }
}

