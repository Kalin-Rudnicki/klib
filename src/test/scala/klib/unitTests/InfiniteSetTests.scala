package klib.unitTests

import org.scalatest.funspec.AnyFunSpec

import klib.utils.InfiniteSet, InfiniteSet._

class InfiniteSetTests extends AnyFunSpec {

  private val Inclusive1: InfiniteSet[Int] = Inclusive(1, 2)
  private val Inclusive2: InfiniteSet[Int] = Inclusive(2, 3)

  private val Exclusive1: InfiniteSet[Int] = Exclusive(1, 2)
  private val Exclusive2: InfiniteSet[Int] = Exclusive(2, 3)

  describe("operations") {

    describe("Inclusive1") {
      val _1 = Inclusive1

      it("~") {
        assertResult(Exclusive(1, 2))(_1.~)
      }

      describe("contains") {

        it("1") {
          assertResult(true)(_1.contains(1))
        }

        it("2") {
          assertResult(true)(_1.contains(2))
        }

        it("3") {
          assertResult(false)(_1.contains(3))
        }

        it("4") {
          assertResult(false)(_1.contains(4))
        }

      }

      describe("Inclusive1") {
        val _2 = Inclusive1

        it("explicit") {
          assertResult(Set(1, 2))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Inclusive(1, 2))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(1, 2))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive())(_1 &~ _2)
        }

      }

      describe("Inclusive2") {
        val _2 = Inclusive2

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Inclusive(1, 2, 3))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(2))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(1))(_1 &~ _2)
        }

      }

      describe("Exclusive1") {
        val _2 = Exclusive1

        it("explicit") {
          assertResult(Set(1, 2))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive())(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive())(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(1, 2))(_1 &~ _2)
        }

      }

      describe("Exclusive2") {
        val _2 = Exclusive2

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(3))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(1))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(2))(_1 &~ _2)
        }

      }
    }

    describe("Inclusive2") {
      val _1 = Inclusive2

      it("~") {
        assertResult(Exclusive(2, 3))(_1.~)
      }

      describe("contains") {

        it("1") {
          assertResult(false)(_1.contains(1))
        }

        it("2") {
          assertResult(true)(_1.contains(2))
        }

        it("3") {
          assertResult(true)(_1.contains(3))
        }

        it("4") {
          assertResult(false)(_1.contains(4))
        }

      }

      describe("Inclusive1") {
        val _2 = Inclusive1

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Inclusive(1, 2, 3))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(2))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(3))(_1 &~ _2)
        }

      }

      describe("Inclusive2") {
        val _2 = Inclusive2

        it("explicit") {
          assertResult(Set(2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Inclusive(2, 3))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(2, 3))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive())(_1 &~ _2)
        }

      }

      describe("Exclusive1") {
        val _2 = Exclusive1

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(1))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(3))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(2))(_1 &~ _2)
        }

      }

      describe("Exclusive2") {
        val _2 = Exclusive2

        it("explicit") {
          assertResult(Set(2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive())(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive())(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(2, 3))(_1 &~ _2)
        }

      }
    }

    describe("Exclusive1") {
      val _1 = Exclusive1

      it("~") {
        assertResult(Inclusive(1, 2))(_1.~)
      }

      describe("contains") {

        it("1") {
          assertResult(false)(_1.contains(1))
        }

        it("2") {
          assertResult(false)(_1.contains(2))
        }

        it("3") {
          assertResult(true)(_1.contains(3))
        }

        it("4") {
          assertResult(true)(_1.contains(4))
        }

      }

      describe("Inclusive1") {
        val _2 = Inclusive1

        it("explicit") {
          assertResult(Set(1, 2))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive())(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive())(_1 & _2)
        }

        it("&~") {
          assertResult(Exclusive(1, 2))(_1 &~ _2)
        }

      }

      describe("Inclusive2") {
        val _2 = Inclusive2

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(1))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(3))(_1 & _2)
        }

        it("&~") {
          assertResult(Exclusive(1, 2, 3))(_1 &~ _2)
        }

      }

      describe("Exclusive1") {
        val _2 = Exclusive1

        it("explicit") {
          assertResult(Set(1, 2))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(1, 2))(_1 | _2)
        }

        it("&") {
          assertResult(Exclusive(1, 2))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive())(_1 &~ _2)
        }

      }

      describe("Exclusive2") {
        val _2 = Exclusive2

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(2))(_1 | _2)
        }

        it("&") {
          assertResult(Exclusive(1, 2, 3))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(3))(_1 &~ _2)
        }

      }
    }

    describe("Exclusive2") {
      val _1 = Exclusive2

      it("~") {
        assertResult(Inclusive(2, 3))(_1.~)
      }

      describe("contains") {

        it("1") {
          assertResult(true)(_1.contains(1))
        }

        it("2") {
          assertResult(false)(_1.contains(2))
        }

        it("3") {
          assertResult(false)(_1.contains(3))
        }

        it("4") {
          assertResult(true)(_1.contains(4))
        }

      }

      describe("Inclusive1") {
        val _2 = Inclusive1

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(3))(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive(1))(_1 & _2)
        }

        it("&~") {
          assertResult(Exclusive(1, 2, 3))(_1 &~ _2)
        }

      }

      describe("Inclusive2") {
        val _2 = Inclusive2

        it("explicit") {
          assertResult(Set(2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive())(_1 | _2)
        }

        it("&") {
          assertResult(Inclusive())(_1 & _2)
        }

        it("&~") {
          assertResult(Exclusive(2, 3))(_1 &~ _2)
        }

      }

      describe("Exclusive1") {
        val _2 = Exclusive1

        it("explicit") {
          assertResult(Set(1, 2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(2))(_1 | _2)
        }

        it("&") {
          assertResult(Exclusive(1, 2, 3))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive(1))(_1 &~ _2)
        }

      }

      describe("Exclusive2") {
        val _2 = Exclusive2

        it("explicit") {
          assertResult(Set(2, 3))(InfiniteSet.explicit(_1, _2))
        }

        it("|") {
          assertResult(Exclusive(2, 3))(_1 | _2)
        }

        it("&") {
          assertResult(Exclusive(2, 3))(_1 & _2)
        }

        it("&~") {
          assertResult(Inclusive())(_1 &~ _2)
        }

      }
    }

  }

  describe("set-theory") {

    def test(
        name: String,
        set: InfiniteSet[Int],
        otherSet: InfiniteSet[Int],
    ): Unit =
      describe(name) {

        describe("basic") {

          it("set == set") {
            assert(set == set)
          }

          it("set.~.~ == set") {
            assert(set.~.~ == set)
          }

          it("set.~.explicit == set.explicit") {
            assert(set.~.explicit == set.explicit)
          }

        }

        describe("associativity") {

          it("(set | otherSet) == (otherSet | set)") {
            assume(set != otherSet)
            assert((set | otherSet) == (otherSet | set))
          }

          it("(set & otherSet) == (otherSet & set)") {
            assume(set != otherSet)
            assert((set & otherSet) == (otherSet & set))
          }

          it("(set &~ otherSet) != (otherSet &~ set)") {
            assume(set != otherSet)
            assert((set &~ otherSet) != (otherSet &~ set))
          }

        }

        describe("self-operations") {

          it("(set | set) == set") {
            assert((set | set) == set)
          }

          it("(set & set) == set") {
            assert((set & set) == set)
          }

          it("(set &~ set) == InfiniteSet.empty") {
            assert((set &~ set) == InfiniteSet.empty)
          }

        }

        describe("empty-operations") {

          it("(set | InfiniteSet.empty) == set") {
            assert((set | InfiniteSet.empty) == set)
          }

          it("(set & InfiniteSet.empty) == InfiniteSet.empty") {
            assert((set & InfiniteSet.empty) == InfiniteSet.empty)
          }

          it("(set &~ InfiniteSet.empty) == set") {
            assert((set &~ InfiniteSet.empty) == set)
          }

          it("(InfiniteSet.empty &~ set) == InfiniteSet.empty") {
            assert((InfiniteSet.empty &~ set) == InfiniteSet.empty)
          }

        }

        describe("full-operations") {

          it("(set | InfiniteSet.full) == InfiniteSet.full") {
            assert((set | InfiniteSet.full) == InfiniteSet.full)
          }

          it("(set & InfiniteSet.full) == set") {
            assert((set & InfiniteSet.full) == set)
          }

          it("(set &~ InfiniteSet.full) == InfiniteSet.empty") {
            assert((set &~ InfiniteSet.full) == InfiniteSet.empty)
          }

          it("(InfiniteSet.full &~ set) == set.~") {
            assert((InfiniteSet.full &~ set) == set.~)
          }

        }

      }

    test("Inclusive1", Inclusive1, Exclusive2)
    test("Inclusive2", Inclusive2, Exclusive1)
    test("Exclusive1", Exclusive1, Inclusive2)
    test("Exclusive2", Exclusive2, Inclusive1)

  }

}
