package jp.yaki.scala.study.functionaldesign

object ChapterFour extends App {

  override def main(args: Array[String]): Unit = {
    //    print(failingFn(12))
    //    print(failingFn2(12))
    //    print(mean(Seq(1, 2, 3, 4, 5)))
    //    print(mean_1(IndexedSeq(), 1.1))
    print(mean_2(Seq(1, 2, 3, 4)))
  }

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    } catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else
      Some(xs.sum / xs.length)
  }

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = {
    if (xs.isEmpty)
      onEmpty
    else
      xs.sum / xs.length
  }

  def mean_2(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    // 違いがわかんね
    mean(xs).map(m => mean(xs.map(x => math.pow(x - m, 2))))
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def absO: Option[Double] => Option[Double] = {
    lift(math.abs)
  }

  def mapKkz[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(o) => Some(f(o))
      case None    => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(o) => f(o)
      case None    => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None    => default
      case Some(a) => a
    }
  }

  // 答え見た
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None    => ob
      case Some(a) => Some(a)
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => Some(a)
      case _               => None
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

case class Employee(name: String, department: String) {
  def lookupByName(name: String): Option[Employee] = {
    Some(Employee(name, ""))
  }
  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
}
