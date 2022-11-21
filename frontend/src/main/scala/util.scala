package de.cfaed.kitten

extension[A] (a: A)
  def pp(hint: String = ""): A = {
    println(hint + a.toString)
    a
  }

extension[A, B] (eithers: List[Either[A, B]]) def flattenList: Either[A, List[B]] =
  eithers.partition(_.isLeft) match
    case (Nil, rights) => Right(for (Right(b) <- rights) yield b)
    case (hd :: _, _) => Left(hd.left.get)

extension[A, B] (self: Either[A, B]) def withFilter(filter: B => Boolean): Either[A, B] = self match
  case right@Right(value) => if filter(value) then right else throw new MatchError(value)
  case left => left
