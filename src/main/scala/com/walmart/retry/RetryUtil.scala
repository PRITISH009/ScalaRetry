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
  sealed case class TransientExceptions(additionalExceptions: Seq[Throwable] = Seq.empty[Throwable]) {
    private val defaultExceptions = Seq(SocketException(), InterruptedIOException(), TransientInterruptedException())
    val exceptionTypeList: Seq[String] = extractType((defaultExceptions ++ additionalExceptions) : _*)

    private def extractType(exceptionList: Throwable*): Seq[String] = exceptionList.map(extractType)

    private def extractType(exception: Throwable): String = exception.getClass.toString.split('.').last

    def ->:(exception:Throwable): Boolean = exceptionTypeList.contains(extractType(exception))
  }

  /**
   * Retry Function to retry any function passed as a parameter along with other arguments like
   * numRetries, retryFactor, retryIncrementer and additionalExceptions which if encountered still calls for a retry.
   *
   * Example -
   *
   *
   * if the following is a function that needs to be retried -
   *
   *
   * def connection(port: Int, IpAddress: String): Boolean = ???
   *
   *
   * Then to retry "connection" function we need to pass it via a lambda function as follows -
   *
   *
   * // if you want to use default parameters for retry
   *
   * retry(() => connection(8080,"0.0.0.0"))
   *
   *
   * // if you want use other retry arguments
   *
   * retry(function = () => connection(8080, "0.0.0.0"), numRetries = 5, retryFactor = 5, retryIncrementer = 3)
   *
   *
   * The return type of this retry function is Either[Throwable, F] where F is the return type of "connection" i.e Boolean
   * in this case.
   *
   * @param function function to retry
   * @param numRetries number of retries (default value 3)
   * @param retryFactor wait time before the next retry (default value 10 secs)
   * @param retryIncrementer multiplication factor to increment retryFactor for each retry.
   * @param additionalExceptions List of Additional Exceptions for which retry needs to be processed.
   * @tparam F Generic Type to represent return type of the function passed. (auto inferred)
   * @return Either an Exception (in case of non transient exceptions or exceptions occurring even after retry) or the function result.
   */
  @tailrec
  final def retry[F](function: () => F,
                     numRetries: Int = 3,
                     retryFactor: Int = 10,
                     retryIncrementer: Int = 2,
                     additionalExceptions: Seq[Throwable] = Seq.empty[Throwable]): Either[Throwable, F] =
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
      case Failure(exception) if exception ->: TransientExceptions(additionalExceptions) & numRetries > 0 => {
        println(s"Exception - ${exception.getMessage} Caught")
        println(s"Waiting for ${retryFactor} secs\n")
        Thread.sleep(retryFactor * 1000)
        println("Retrying...\n")
        retry(function, numRetries - 1, retryFactor * retryIncrementer, retryIncrementer, additionalExceptions)
      }
      case Failure(exception) => {
        println("Exception Caught even after retrying")
        println("Returning Exception")
        Left(exception)
      }
    }
}
