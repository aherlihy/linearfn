package linearfn

case class Wrap[T](v: T)

// Make Wrap liftable by providing a Liftable instance
given RestrictedSelectable.Restricted.Liftable[Wrap] with
  def map[A, B](fa: Wrap[A])(f: A => B): Wrap[B] = Wrap(f(fa.v))



@main def main() = {
  println("Linear Function Library")

}
