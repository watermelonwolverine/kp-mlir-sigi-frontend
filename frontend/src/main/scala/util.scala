package de.cfaed.kitten

import scala.collection.mutable.ListBuffer

extension[A] (self: A)
  def pp(hint: String = ""): A = {
    println(hint + self.toString)
    self
  }

  def also(f: A => Unit): self.type = {
    f(self)
    self
  }

extension[A, B] (eithers: List[Either[A, B]]) def flattenList: Either[A, List[B]] =
  eithers.partition(_.isLeft) match
    case (Nil, rights) => Right(for (Right(b) <- rights) yield b)
    case (hd :: _, _) => Left(hd.left.get)

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
