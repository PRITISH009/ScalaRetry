package com.walmart.retry

import java.io.InterruptedIOException
import java.net.SocketException
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object RetryUtil extends App {
  /**
   * Sealed Custom InterruptedException for Scala Try to process in Failure
   * @param message Exception Message
   */
  sealed case class TransientInterruptedException(message: String = "") extends Exception(message)

  /**
   * Grouped Transient Exceptions with functionality to add more exceptions to retry on.
   * @param additionalExceptions Seq of Exception Objects.
   */
  sealed case class RetryExceptionList(exceptionList: Seq[Exception] = Seq.empty[Exception]) {

    val exceptionTypeList: Seq[String] = extractType(exceptionList : _*)

    private def extractType(exceptionList: Throwable*): Seq[String] = exceptionList.map(extractType)

    private def extractType(exception: Throwable): String = exception.getClass.toString.split('.').last

    def ->:(exception:Exception): Boolean = exceptionTypeList.contains(extractType(exception))
  }

  /**
   * Retry Function to retry any function passed as a parameter along with other arguments like
   * numRetries, retryFactor, retryIncrementer and exceptionList which if encountered calls for a retry.
   *
   * Example -
   *
   *
   * if the following is a function that needs to be retried -
   *
   *
   * def connection(port: Int, IpAddress: String): Boolean = ???
   *
   * You have the ability to create a partially applied function as -
   *
   * you can define an exceptionList on which you want to create a partially applied function as follows -
   *
   * val exceptionList = Seq(SocketException(), InterruptedException(), InterruptedIOException())
   *
   * val partiallyAppliedRetryFunction = retry(numRetries = 4, retryFactor = 20000, exceptionList = exceptionList)
   *
   * this partially applied function can be used to retry any function with the same parameter list.
   *
   * partiallyAppliedRetryFunction(() => connection)
   *
   * @param numRetries number of retries (default value 3)
   * @param retryFactor wait time before the next retry in milli seconds (default value 10 secs)
   * @param retryIncrementer multiplication factor to increment retryFactor for each retry.
   * @param exceptionList List of Exceptions for which retry needs to be processed.
   * @param function function to retry
   * @tparam F Generic Type to represent return type of the function passed. (auto inferred)
   * @return Either an exception or the function result after specified number of retries (if required)
   */
  @tailrec
  final def retry[F](numRetries: Int = 3,
                     retryFactor: Long = 10000,
                     retryIncrementer: Float = 2.0,
                     exceptionList: Seq[Exception] = Seq.empty[Exception])
                    (function: () => F): Either[Exception, F] =
    Try {
      try {
        println("Trying Function\n")
        function()
      } catch case e: InterruptedException => {
        println("Interrupted Exception Caught")
        throw TransientInterruptedException(e.getMessage)
      }
    } match {
      case Success(x) => Right(x)
      case Failure(exception: Exception) if exception ->: RetryExceptionList(exceptionList) & numRetries > 0 => {
        println(s"Exception - ${exception.getMessage} Caught")
        println(s"Waiting for ${retryFactor} secs\n")
        Thread.sleep(retryFactor * 1000)
        println("Retrying...\n")
        retry(numRetries - 1, (retryFactor * retryIncrementer).toLong, retryIncrementer, exceptionList)(function)
      }
      case Failure(exception: Exception) => {
        println("Exception Caught even after retrying")
        println("Returning Exception")
        Left(exception)
      }
    }
}
