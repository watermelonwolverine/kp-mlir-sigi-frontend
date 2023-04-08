package de.cfaed.sigi

import scala.collection.mutable.ListBuffer

extension[A] (self: A)
  def pp(hint: String = ""): A = {
    val prefix = if hint.nonEmpty then s"$hint: " else ""
    val selfToStr = if self == null then "null" else self.toString
    System.err.println(prefix + selfToStr)
    self
  }

  def also(f: A => Unit): self.type = {
    f(self)
    self
  }

extension[A, B] (eithers: List[Either[A, B]]) def flattenList(using mergeFun: (A, List[A]) => A): Either[A, List[B]] =
  eithers.partition(_.isLeft) match
    case (Nil, rights) => Right(rights.collect { case Right(b) => b })
    case (Left(hd) :: rest, _) => Left(mergeFun(hd, rest.collect { case Left(a) => a }))

// note: this is  not a general purpose implementation, as it requires a total pattern.
// we can't conjure a Left from nowhere
extension[A, B] (self: Either[A, B]) def withFilter(filter: B => Boolean): Either[A, B] = self match
  case right@Right(value) => if filter(value) then right else throw new MatchError(value)
  case left => left

extension[A] (self: List[A])

  /** Returns (self.prefix, self.suffix) where
    * the suffix has the given length.
    */
  def splitAtRight(numElemsRight: Int): (List[A], List[A]) =
    val split = math.max(self.length - numElemsRight, 0)
    self.splitAt(split)
