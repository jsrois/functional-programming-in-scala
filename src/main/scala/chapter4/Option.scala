package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap((x: A) => if (f(x)) Some(x) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))

  def lift[A,B](f: A => B): Option[A] => Option[B] =
    _.map(f)

}


