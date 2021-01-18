package klib.utils

sealed trait InfiniteSet[T] {

  val explicit: Set[T]

  def invert: InfiniteSet[T]
  def union(that: InfiniteSet[T]): InfiniteSet[T]
  def intersection(that: InfiniteSet[T]): InfiniteSet[T]
  def disjunction(that: InfiniteSet[T]): InfiniteSet[T]

  def contains(t: T): Boolean

  // aliases
  @inline def ~ : InfiniteSet[T] = invert
  @inline def |(that: InfiniteSet[T]): InfiniteSet[T] = union(that)
  @inline def &(that: InfiniteSet[T]): InfiniteSet[T] = intersection(that)
  @inline def &~(that: InfiniteSet[T]): InfiniteSet[T] = disjunction(that)

}

object InfiniteSet {

  def explicit[T](sets: InfiniteSet[T]*): Set[T] =
    sets.toSet.flatMap((s: InfiniteSet[T]) => s.explicit)

  final case class Inclusive[T](explicit: Set[T]) extends InfiniteSet[T] {

    override def invert: InfiniteSet[T] =
      Exclusive(explicit)

    override def union(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Inclusive(this.explicit | that.explicit)
        case that @ Exclusive(_) =>
          Exclusive(that.explicit &~ this.explicit)
      }

    override def intersection(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Inclusive(this.explicit & that.explicit)
        case that @ Exclusive(_) =>
          Inclusive(this.explicit &~ that.explicit)
      }

    override def disjunction(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Inclusive(this.explicit &~ that.explicit)
        case that @ Exclusive(_) =>
          Inclusive(this.explicit & that.explicit)
      }

    override def contains(t: T): Boolean =
      explicit.contains(t)

  }

  object Inclusive {

    def apply[T](ts: T*): Inclusive[T] =
      Inclusive(ts.toSet)

  }

  final case class Exclusive[T](explicit: Set[T]) extends InfiniteSet[T] {

    override def invert: InfiniteSet[T] =
      Inclusive(explicit)

    override def union(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Exclusive(this.explicit &~ that.explicit)
        case that @ Exclusive(_) =>
          Exclusive(this.explicit & that.explicit)
      }

    override def intersection(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Inclusive(that.explicit &~ this.explicit)
        case that @ Exclusive(_) =>
          Exclusive(this.explicit | that.explicit)
      }

    override def disjunction(that: InfiniteSet[T]): InfiniteSet[T] =
      that match {
        case that @ Inclusive(_) =>
          Exclusive(this.explicit | that.explicit)
        case that @ Exclusive(_) =>
          Inclusive(that.explicit &~ this.explicit)
      }

    override def contains(t: T): Boolean =
      !explicit.contains(t)

  }

  object Exclusive {

    def apply[T](ts: T*): Exclusive[T] =
      Exclusive(ts.toSet)

  }

}
