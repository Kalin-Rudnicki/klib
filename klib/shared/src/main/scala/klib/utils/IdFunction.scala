package klib.utils

object IdFunction {

  type K0[K] = K => K
  object K0 {
    def identity[K]: K0[K] = (k: K) => k
    def apply[K](f: K => K): K0[K] = f
  }

  type K1[K[_]] = [A] => K[A] => K[A]
  object K1 {
    def identity[K[_]]: K1[K] = [A] => (k: K[A]) => k
    def apply[K[_]](f: [A] => K[A] => K[A]): K1[K] = f
  }

  type K2[K[_, _]] = [A, B] => K[A, B] => K[A, B]
  object K2 {
    def identity[K[_, _]]: K2[K] = [A, B] => (k: K[A, B]) => k
    def apply[K[_, _]](f: [A, B] => K[A, B] => K[A, B]): K2[K] = f
  }

  type K3[K[_, _, _]] = [A, B, C] => K[A, B, C] => K[A, B, C]
  object K3 {
    def identity[K[_, _, _]]: K3[K] = [A, B, C] => (k: K[A, B, C]) => k
    def apply[K[_, _, _]](f: [A, B, C] => K[A, B, C] => K[A, B, C]): K3[K] = f
  }

}
