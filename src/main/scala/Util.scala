package com.test

import scala.util.Random

object Util {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000 + "ms")
    result
  }

  def getShuffledArray(n: Int, limit: Int = 0) :Array[Int] ={
    val l = if (limit == 0) n else limit
    Random.shuffle(1 to n).take(l).toArray
  }

}
