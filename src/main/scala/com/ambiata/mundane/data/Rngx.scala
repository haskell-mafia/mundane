package com.ambiata.mundane.data

import scalaz._, Scalaz._
import NonEmptyList._
import com.nicta.rng._
import Rng._

/**
 * This object contains a few additional functions on top of the Rng functionalities
 */
object Rngx {

  /*
   * XORShift random number generator
   */
  var seed: Long = scala.util.Random.nextLong

  def nextLong: Long = {
    seed = seed ^ (seed << 21)
    seed = seed ^ (seed >>> 35)
    seed = seed ^ (seed << 4)
    seed 
  }

  def nextDouble: Double = ((nextLong.toDouble / Long.MaxValue) + 1.0) / 2.0

  def shuffle[A](in: Seq[A]): Seq[A] = in sortBy(_ => nextLong)

  /**
   * filter for lists or options
   */
  implicit class FilteredMonadPlus[A, M[_] : MonadPlus](generator: Rng[M[A]]) {
    /** @return a Rng which might or not produce a value based on a predicate */
    def filter(condition: A => Boolean): Rng[M[A]]    = generator.map(_.filter(condition))
    def filterNot(condition: A => Boolean): Rng[M[A]] = filter(e => !condition(e))
  }

  /**
   * @return a generator for a Stream[A] based on a generator of A
   */
  implicit class Infinite[A](generator: Rng[A]) {
    def infinite: Rng[EphemeralStream[A]] = {
      def stream: EphemeralStream[A] = EphemeralStream.cons(generator.runIO, stream)
      oneof(stream)
    }
  }

  /**
   * @return a generator a (possibly empty) subset of values from a list of values
   *         if the size is not specified, then it is randomly chosen
   */
  implicit class ListSubset[A : Equal](values: List[A]) {
    /** randomly select n at most distinct values */
    def subset(size: Size): Rng[List[A]] =
      values match {
        case Nil          => oneof(Nil)
        case head :: tail => nel(head, tail).subset(size)
      }
    /** randomly select n exactly distinct values */
    def subset(n: Int): Rng[List[A]] =
      values match {
        case Nil          => oneof(Nil)
        case head :: tail => nel(head, tail).subset(n)
      }
  }

  /**
   * @return a generator a non empty subset of values from a list of values
   */
  implicit class Subset[A : Equal](values: NonEmptyList[A]) {
    val equalA = implicitly[Equal[A]]

    /**
     * randomly select n at most distinct values
     * if the size is not specified a random number of values between 0 and values.size will be selected
     */
    def subset(size: Size): Rng[List[A]] = {
      size.value.map { s =>
        if (s <= 0) oneof(Nil)
        else        subset1(size).map(_.list)
      } getOrElse {
        if (values.list.isEmpty) oneof(Nil)
        else                     subset1(size).map(_.list)
      }
    }

    /**
     * select exactly n values from the list
     */
    def subset(n: Int): Rng[List[A]] = {
      if (n <= 0) oneof(Nil)
      else        subset1(n).map(_.list)
    }

    /**
     * select 1 to n at most distinct values
     * if the size is not specified there will be between 1 and values.size values chosen
     */
    def subset1(size: Size): Rng[NonEmptyList[A]] = {
      for {
        n      <- size.value.map(chooseint(1, _)).getOrElse(chooseint(1, values.size))
        result <- values.subset1(n)
      } yield result
    }

    /**
     * select exactly n distinct values (if 1 <= n <= values.size)
     * This method will select at least one value
     */
    def subset1(n: Int): Rng[NonEmptyList[A]] = {
      for {
        one    <- oneofL(values)
        others <- {
          val (ones, rest) = values.list.partition(v => equalA.equal(one, v))
          (ones.drop(1) ++ rest).subset(n - 1)
        }
      } yield nel(one, others)
    }
  }

  /**
   * create a list of As, with a size specified by another generator
   * Note that this gives different results than:
   *
   * for {
   *   size <- n
   *   as   <- generator.list(size)
   * } yield as
   */
  implicit class ListOfN[A](generator: Rng[A]) {

    def list(n: Rng[Int]): Rng[List[A]] = for {
      size <- n
      as   <- generator.fill(size)
    } yield as
  }

  /** allow to write 1 to 10 when a Rng[Int] is expected */
  implicit def RangeToRngInt(range: Range): Rng[Int] = chooseint(range.start, range.end)

  /** allow use a single value A where a Rng[A] is expected */
  implicit def valueToRng[A](value: A): Rng[A] = oneof(value)

  /**
   * add a cyclic method to a list in order to create an infinite stream with the list
   * values in cyclic order
   *
   * Daniel Spiewak saves the day, see: http://stackoverflow.com/questions/2097851/scala-repeat-a-finite-list-infinitely
   */
  implicit class Cyclic[A](list: List[A]) {
    def cyclic: Stream[A] = {
      def inner(proj: Seq[A]): Stream[A] = {
        if (proj.isEmpty) inner(list)
        else               Stream.cons(proj.head, inner(proj.tail))
      }
      if (list.isEmpty) Stream.empty
      else              inner(list)
    }
  }

  implicit class RunIO[A](generator: Rng[A]) {
    def runIO = generator.run.unsafePerformIO
  }

}
