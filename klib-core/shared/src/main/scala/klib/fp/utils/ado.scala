// DO NOT EDIT : This file was automatically generated
package klib.fp.utils

// format: off
import klib.fp.typeclass.Applicative
import klib.fp.typeclass.Implicits._

final class ado[T[_]: Applicative] {
  def join[
    T1,
    T2,
  ](
   _1: T[T1],
   _2: T[T2],
  ): T[(
    T1,
    T2,
  )] =
   _1.aJoin(_2).map {
      case (_1, _2) =>
        (_1, _2)
    }
  
  def join[
    T1,
    T2,
    T3,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
  ): T[(
    T1,
    T2,
    T3,
  )] =
   _1.aJoin(_2).aJoin(_3).map {
      case ((_1, _2), _3) =>
        (_1, _2, _3)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
  ): T[(
    T1,
    T2,
    T3,
    T4,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).map {
      case (((_1, _2), _3), _4) =>
        (_1, _2, _3, _4)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).map {
      case ((((_1, _2), _3), _4), _5) =>
        (_1, _2, _3, _4, _5)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).map {
      case (((((_1, _2), _3), _4), _5), _6) =>
        (_1, _2, _3, _4, _5, _6)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).map {
      case ((((((_1, _2), _3), _4), _5), _6), _7) =>
        (_1, _2, _3, _4, _5, _6, _7)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).map {
      case (((((((_1, _2), _3), _4), _5), _6), _7), _8) =>
        (_1, _2, _3, _4, _5, _6, _7, _8)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).map {
      case ((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).map {
      case (((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).map {
      case ((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).map {
      case (((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).map {
      case ((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).map {
      case (((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).map {
      case ((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).map {
      case (((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).map {
      case ((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
   _18: T[T18],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).aJoin(_18).map {
      case (((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17), _18) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
   _18: T[T18],
   _19: T[T19],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).aJoin(_18).aJoin(_19).map {
      case ((((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17), _18), _19) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
   _18: T[T18],
   _19: T[T19],
   _20: T[T20],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).aJoin(_18).aJoin(_19).aJoin(_20).map {
      case (((((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17), _18), _19), _20) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
   _18: T[T18],
   _19: T[T19],
   _20: T[T20],
   _21: T[T21],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).aJoin(_18).aJoin(_19).aJoin(_20).aJoin(_21).map {
      case ((((((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17), _18), _19), _20), _21) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21)
    }
  
  def join[
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22,
  ](
   _1: T[T1],
   _2: T[T2],
   _3: T[T3],
   _4: T[T4],
   _5: T[T5],
   _6: T[T6],
   _7: T[T7],
   _8: T[T8],
   _9: T[T9],
   _10: T[T10],
   _11: T[T11],
   _12: T[T12],
   _13: T[T13],
   _14: T[T14],
   _15: T[T15],
   _16: T[T16],
   _17: T[T17],
   _18: T[T18],
   _19: T[T19],
   _20: T[T20],
   _21: T[T21],
   _22: T[T22],
  ): T[(
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22,
  )] =
   _1.aJoin(_2).aJoin(_3).aJoin(_4).aJoin(_5).aJoin(_6).aJoin(_7).aJoin(_8).aJoin(_9).aJoin(_10).aJoin(_11).aJoin(_12).aJoin(_13).aJoin(_14).aJoin(_15).aJoin(_16).aJoin(_17).aJoin(_18).aJoin(_19).aJoin(_20).aJoin(_21).aJoin(_22).map {
      case (((((((((((((((((((((_1, _2), _3), _4), _5), _6), _7), _8), _9), _10), _11), _12), _13), _14), _15), _16), _17), _18), _19), _20), _21), _22) =>
        (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22)
    }
  
}

object ado {

  def apply[T[_]: Applicative]: ado[T] =
    new ado[T]

}

// format: on