package com.walmart.retry

import java.io.InterruptedIOException
import java.net.SocketException
import scala.util.Random
import com.walmart.retry.RetryUtil.retry

object RetryUsage extends App {
  // Flaky Function
  def flakyFunction: String = {
    val connectionStatus = Random.nextInt(4)
    connectionStatus match {
      case 0 => "Connection Success"
      case 1 => {
        println("Throwing Interrupted Exception")
        throw new InterruptedException("Interrupted Exception")
      }
      case 2 => {
        println("Throwing Interrupted IO Exception")
        throw new InterruptedIOException("Interrupted IO Exception")
      }
      case 3 => {
        println("Throwing Connection Unstable Exception")
        throw new SocketException("Connection Unstable")
      }
    }
  }

  retry(exceptionList = Seq(InterruptedException(), InterruptedIOException(), SocketException()))(() => flakyFunction) match {
    case Left(exception: Exception) => println(exception.getMessage)
    case Right(x: String) => println(x)
  }
  
  val exceptionList = Seq(InterruptedException(), InterruptedIOException(), SocketException())
  
  def retryPartialFunction = retry(exceptionList = exceptionList)
  // The above function could be used as per the user's desires.
  
}
