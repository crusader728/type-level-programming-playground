package playground

import shapeless.Lazy

import scala.reflect.runtime.universe._

object NQueens {

  def typeInfo[T](t: T)(implicit tag: TypeTag[T]): String = {
    tag.toString()
      .replace("playground.NQueens.", "")
      .replace("HNil)", "HNil)\n")
  }

  trait HList

  trait HNil extends HList

  trait ::[H, T <: HList] extends HList

  trait Nat

  trait Z extends Nat

  trait Succ[Z <: Nat] extends Nat

  type _0 = Z
  type _1 = Succ[Z]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]

  trait First[L <: HList] {
    type Result
  }

  object First {
    def apply[L <: HList](implicit f: First[L]): Aux[L, f.Result] = f

    type Aux[L <: HList, H] = First[L] {type Result = H}

    implicit def f[H, T <: HList]: Aux[::[H, T], H] = new First[H :: T] {
      override type Result = H
    }
  }

  trait Concat[A <: HList, B <: HList] {
    type Result <: HList
  }

  object Concat {
    type Aux[A <: HList, B <: HList, C <: HList] = Concat[A, B] {type Result = C}

    implicit def c0[A <: HList]: Aux[HNil, A, A] = new Concat[HNil, A] {
      override type Result = A
    }

    implicit def c1[A, B <: HList, C <: HList](
                                                implicit c: Concat[B, C]
                                              ): Aux[::[A, B], C, ::[A, c.Result]] = new Concat[A :: B, C] {
      override type Result = A :: c.Result
    }
  }

  trait Flatten[Ls <: HList] {
    type Result <: HList
  }

  object Flatten {
    def apply[Ls <: HList](implicit ca: Flatten[Ls]): Aux[Ls, ca.Result] = ca

    type Aux[Ls <: HList, L <: HList] = Flatten[Ls] {type Result = L}
    implicit val c0: Aux[HNil, HNil] = new Flatten[HNil] {
      override type Result = HNil
    }

    implicit def c1[Chunk <: HList, Rest <: HList, Acc <: HList](
                                                                  implicit
                                                                  ca: Aux[Rest, Acc],
                                                                  c: Concat[Chunk, Acc]): Aux[::[Chunk, Rest], c.Result] = new Flatten[::[Chunk, Rest]] {
      override type Result = c.Result
    }
  }

  trait Bool

  class True extends Bool

  class False extends Bool

  trait AnyTrue[L] {
    type Result <: Bool
  }

  object AnyTrue {
    type Aux[L <: HList, B <: Bool] = AnyTrue[L] {type Result = B}
    implicit val a0: Aux[HNil, False] = new AnyTrue[HNil] {
      override type Result = False
    }

    implicit def a1[T <: HList]: Aux[True :: T, True] = new AnyTrue[True :: T] {
      override type Result = True
    }

    implicit def a2[T <: HList](
                                 implicit
                                 a: AnyTrue[T]): Aux[False :: T, a.Result] = new AnyTrue[False :: T] {
      override type Result = a.Result
    }
  }

  trait Not[A <: Bool] {
    type Result <: Bool
  }

  object Not {
    type Aux[A <: Bool, B <: Bool] = Not[A] {type Result = B}
    implicit val not0: Aux[True, False] = new Not[True] {
      override type Result = False
    }
    implicit val not1: Aux[False, True] = new Not[False] {
      override type Result = True
    }
  }

  trait Or[A <: Bool, B <: Bool] {
    type Result <: Bool
  }

  object Or {
    type Aux[A <: Bool, B <: Bool, C <: Bool] = Or[A, B] {type Result = C}
    implicit val or0: Aux[True, True, True] = new Or[True, True] {
      override type Result = True
    }
    implicit val or1: Aux[True, False, True] = new Or[True, False] {
      override type Result = True
    }
    implicit val or2: Aux[False, True, True] = new Or[False, True] {
      override type Result = True
    }
    implicit val or3: Aux[False, False, False] = new Or[False, False] {
      override type Result = False
    }
  }

  trait Eq[A <: Nat, B <: Nat] {
    type Result <: Bool
  }

  object Eq {
    type Aux[A <: Nat, B <: Nat, C <: Bool] = Eq[A, B] {type Result = C}

    implicit val eq0: Aux[_0, _0, True] = new Eq[_0, _0] {
      override type Result = True
    }

    implicit def eq1[A <: Nat]: Aux[_0, Succ[A], False] = new Eq[_0, Succ[A]] {
      override type Result = False
    }

    implicit def eq2[A <: Nat]: Aux[Succ[A], _0, False] = new Eq[Succ[A], _0] {
      override type Result = False
    }

    implicit def eq3[A <: Nat, B <: Nat](implicit e: Eq[A, B]): Aux[Succ[A], Succ[B], e.Result] = new Eq[Succ[A], Succ[B]] {
      override type Result = e.Result
    }
  }

  trait Lt[A <: Nat, B <: Nat] {
    type Result <: Bool
  }

  object Lt {
    type Aux[A <: Nat, B <: Nat, C <: Bool] = Lt[A, B] {type Result = C}
    implicit val lt0: Aux[_0, _0, False] = new Lt[_0, _0] {
      override type Result = False
    }

    implicit def lt1[A <: Nat]: Aux[_0, Succ[A], True] = new Lt[_0, Succ[A]] {
      override type Result = True
    }

    implicit def lt2[A <: Nat]: Aux[Succ[A], _0, False] = new Lt[Succ[A], _0] {
      override type Result = False
    }

    implicit def lt3[A <: Nat, B <: Nat](
                                          implicit l: Lt[A, B]): Aux[Succ[A], Succ[B], l.Result] = new Lt[Succ[A], Succ[B]] {
      override type Result = l.Result
    }
  }

  trait AbsDiff[A <: Nat, B <: Nat] {
    type Result <: Nat
  }

  object AbsDiff {
    type Aux[A <: Nat, B <: Nat, C <: Nat] = AbsDiff[A, B] {type Result = C}
    implicit val a0: Aux[_0, _0, _0] = new AbsDiff[_0, _0] {
      override type Result = _0
    }

    implicit def a1[A <: Nat]: Aux[A, _0, A] = new AbsDiff[A, _0] {
      override type Result = A
    }

    implicit def a2[A <: Nat]: Aux[_0, A, A] = new AbsDiff[_0, A] {
      override type Result = A
    }

    implicit def a3[A <: Nat, B <: Nat](
                                         implicit a: AbsDiff[A, B]): Aux[Succ[A], Succ[B], a.Result] = new AbsDiff[Succ[A], Succ[B]] {
      override type Result = a.Result
    }
  }

  trait Range[A <: Nat] {
    type Result <: HList
  }

  object Range {
    type Aux[A <: Nat, L <: HList] = Range[A] {type Result = L}
    implicit val r0: Aux[_0, HNil] = new Range[_0] {
      override type Result = HNil
    }

    implicit def r1[A <: Nat](implicit r: Range[A]): Aux[Succ[A], A :: r.Result] =
      new Range[Succ[A]] {
        override type Result = A :: r.Result
      }
  }

  trait Func

  trait Apply[F <: Func, A] {
    type Result
  }

  object Apply {
    def apply[F <: Func, A](implicit a: Apply[F, A]): Aux[F, A, a.Result] = a

    type Aux[F <: Func, A, R] = Apply[F, A] {type Result = R}

  }

  trait Conj[L <: HList] extends Func

  object Conj {
    implicit def a0[L <: HList, A]: Apply.Aux[Conj[L], A, A :: L] = new Apply[Conj[L], A] {
      override type Result = A :: L
    }
  }

  trait Map[F <: Func, L <: HList] {
    type Result <: HList
  }

  object Map {
    def apply[F <: Func, L <: HList](implicit m: Map[F, L]): Aux[F, L, m.Result] = m

    type Aux[F <: Func, L <: HList, R <: HList] = Map[F, L] {type Result = R}

    implicit def m0[F <: Func]: Aux[F, HNil, HNil] = new Map[F, HNil] {
      override type Result = HNil
    }

    implicit def m1[F <: Func, Head, Tail <: HList, R, M <: HList](
                                                                    implicit
                                                                    a: Apply[F, Head],
                                                                    m: Lazy[Map.Aux[F, Tail, M]]): Aux[F, Head :: Tail, a.Result :: M] = new Map[F, Head :: Tail] {
      override type Result = a.Result :: M
    }
  }

  trait FlattenMap[F <: Func, L <: HList] {
    type Result <: HList
  }

  object FlattenMap {
    def apply[F <: Func, L <: HList](
                                      implicit mc: FlattenMap[F, L]): Aux[F, L, mc.Result] = mc

    type Aux[F <: Func, L <: HList, R <: HList] = FlattenMap[F, L] {type Result = R}

    implicit def mc0[F <: Func]: Aux[F, HNil, HNil] = new FlattenMap[F, HNil] {
      override type Result = HNil
    }

    implicit def mc1[F <: Func, X, Xs <: HList, Chunks <: HList](
                                                                  implicit
                                                                  m: Map.Aux[F, X :: Xs, Chunks],
                                                                  ca: Flatten[Chunks]): Aux[F, X :: Xs, ca.Result] = new FlattenMap[F, X :: Xs] {
      override type Result = ca.Result
    }
  }

  trait AddIf[B <: Bool, A, L <: HList] {
    type Result <: HList
  }

  object AddIf {
    type Aux[B <: Bool, A, L <: HList, R <: HList] = AddIf[B, A, L] {
      type Result = R
    }

    implicit def ai0[A, L <: HList]: Aux[True, A, L, A :: L] = new AddIf[True, A, L] {
      override type Result = A :: L
    }

    implicit def ai1[A, L <: HList]: Aux[False, A, L, L] = new AddIf[False, A, L] {
      override type Result = L
    }
  }

  trait Filter[F <: Func, L <: HList] {
    type Result <: HList
  }

  object Filter {
    def apply[F <: Func, L <: HList](
                                      implicit f: Filter[F, L]): Aux[F, L, f.Result] = f

    type Aux[F <: Func, L <: HList, R <: HList] = Filter[F, L] {type Result = R}

    implicit def f0[F <: Func]: Aux[F, HNil, HNil] = new Filter[F, HNil] {
      override type Result = HNil
    }

    implicit def f1[F <: Func, H, T <: HList, B <: Bool, FT <: HList](
                                                                       implicit
                                                                       a: Apply.Aux[F, H, B],
                                                                       f: Filter.Aux[F, T, FT],
                                                                       ai: AddIf[B, H, FT]): Aux[F, ::[H, T], ai.Result] = new Filter[F, H :: T] {
      override type Result = ai.Result
    }
  }

  class Queen[X <: Nat, Y <: Nat]

  class QueenX[X <: Nat] extends Func

  object QueenX {
    implicit def qa[X <: Nat, Y <: Nat]: Apply.Aux[QueenX[X], Y, Queen[X, Y]] =
      new Apply[QueenX[X], Y] {
        override type Result = Queen[X, Y]
      }
  }

  trait QueensInRow[Y <: Nat, N <: Nat] {
    type Result <: HList
  }

  object QueensInRow {
    def apply[Y <: Nat, N <: Nat](
                                   implicit q: QueensInRow[Y, N]): Aux[Y, N, q.Result] = q

    type Aux[Y <: Nat, N <: Nat, R <: HList] = QueensInRow[Y, N] {type Result = R}

    implicit def qir0[Y <: Nat, N <: Nat, R <: HList](
                                                       implicit
                                                       r: Range.Aux[N, R],
                                                       m: Map[QueenX[Y], R]): Aux[Y, N, m.Result] = new QueensInRow[Y, N] {
      override type Result = m.Result
    }
  }

  trait Threats[Q1 <: Queen[_, _], Q2 <: Queen[_, _]] {
    type Result <: Bool
  }

  object Threats {
    type Aux[Q1 <: Queen[_, _], Q2 <: Queen[_, _], R <: Bool] =
      Threats[Q1, Q2] {type Result = R}

    implicit def t0[X1 <: Nat,
      Y1 <: Nat,
      X2 <: Nat,
      Y2 <: Nat,
      EqX <: Bool,
      EqY <: Bool,
      EqXY <: Bool,
      DX <: Nat,
      DY <: Nat,
      EqD <: Bool](
                    implicit
                    eqX: Eq.Aux[X1, X2, EqX],
                    eqY: Eq.Aux[Y1, Y2, EqY],
                    or0: Or.Aux[EqX, EqY, EqXY],
                    dx: AbsDiff.Aux[X1, X2, DX],
                    dy: AbsDiff.Aux[Y1, Y2, DY],
                    eqD: Eq.Aux[DX, DY, EqD],
                    res: Or[EqXY, EqD]): Aux[Queen[X1, Y1], Queen[X2, Y2], res.Result] = new Threats[Queen[X1, Y1], Queen[X2, Y2]] {
      override type Result = res.Result
    }
  }

  trait ThreatsQ[Q <: Queen[_, _]] extends Func

  object ThreatsQ {
    implicit def t0[Q1 <: Queen[_, _], Q2 <: Queen[_, _]](
                                                           implicit t: Threats[Q1, Q2]): Apply.Aux[ThreatsQ[Q1], Q2, t.Result] =
      new Apply[ThreatsQ[Q1], Q2] {
        override type Result = t.Result
      }
  }

  trait Safe[Config <: HList, Q <: Queen[_, _]] {
    type Result <: Bool
  }

  object Safe {
    def apply[Config <: HList, Q <: Queen[_, _]](
                                                  implicit s: Safe[Config, Q]): Aux[Config, Q, s.Result] = s

    type Aux[Config <: HList, Q <: Queen[_, _], R] = Safe[Config, Q] {
      type Result = R
    }

    implicit def s0[Config <: HList, Q <: Queen[_, _], L <: HList, T1 <: Bool](
                                                                                implicit
                                                                                m: Map.Aux[ThreatsQ[Q], Config, L],
                                                                                t1: AnyTrue.Aux[L, T1],
                                                                                t2: Not[T1]): Aux[Config, Q, t2.Result] = new Safe[Config, Q] {
      override type Result = t2.Result
    }
  }

  trait SafeL[Config <: HList] extends Func

  object SafeL {
    implicit def a[Config <: HList, Q <: Queen[_, _]](
                                                       implicit
                                                       s: Safe[Config, Q]): Apply.Aux[SafeL[Config], Q, s.Result] = new Apply[SafeL[Config], Q] {
      override type Result = s.Result
    }
  }

  trait AddQueen[N <: Nat, X <: Nat, Config <: HList] {
    type Result <: HList
  }

  object AddQueen {
    def apply[N <: Nat, X <: Nat, Config <: HList](
                                                    implicit a: AddQueen[N, X, Config]): Aux[N, X, Config, a.Result] = a

    type Aux[N <: Nat, X <: Nat, Config <: HList, R <: HList] =
      AddQueen[N, X, Config] {type Result = R}

    implicit def aq0[N <: Nat, X <: Nat, Config <: HList, Qs <: HList, S <: HList](
                                                                                    implicit
                                                                                    qr: QueensInRow.Aux[X, N, Qs],
                                                                                    f: Filter.Aux[SafeL[Config], Qs, S],
                                                                                    m: Map[Conj[Config], S]): Aux[N, X, Config, m.Result] = new AddQueen[N, X, Config] {
      override type Result = m.Result
    }
  }

  trait AddQueenNX[N <: Nat, X <: Nat] extends Func

  object AddQueenNX {
    implicit def a0[N <: Nat, X <: Nat, Config <: HList](
                                                          implicit
                                                          a: AddQueen[N, X, Config]): Apply.Aux[AddQueenNX[N, X], Config, a.Result] =
      new Apply[AddQueenNX[N, X], Config] {
        override type Result = a.Result
      }
  }

  trait AddQueenToAll[N <: Nat, X <: Nat, Configs <: HList] {
    type Result <: HList
  }

  object AddQueenToAll {
    def apply[N <: Nat, X <: Nat, Configs <: HList](
                                                     implicit a: AddQueenToAll[N, X, Configs]): Aux[N, X, Configs, a.Result] = a

    type Aux[N <: Nat, X <: Nat, Configs <: HList, R <: HList] =
      AddQueenToAll[N, X, Configs] {type Result = R}

    implicit def a0[N <: Nat, X <: Nat, Configs <: HList](
                                                           implicit
                                                           m: FlattenMap[AddQueenNX[N, X], Configs]): Aux[N, X, Configs, m.Result] = new AddQueenToAll[N, X, Configs] {
      override type Result = m.Result
    }
  }

  trait AddQueens[N <: Nat, X <: Nat, Configs <: HList] {
    type Result <: HList
  }

  object AddQueens {
    def apply[N <: Nat, X <: Nat, Configs <: HList](implicit a: AddQueens[N, X, Configs]): Aux[N, X, Configs, a.Result] = a

    type Aux[N <: Nat, X <: Nat, Configs <: HList, R <: HList] =
      AddQueens[N, X, Configs] {type Result = R}

    implicit def a0[N <: Nat, X <: Nat, Configs <: HList, LT <: Bool, R <: HList](
                                                                                   implicit
                                                                                   lt: Lt.Aux[X, N, LT],
                                                                                   aq: Lazy[AddQueensIf.Aux[LT, N, X, Configs, R]])
    : Aux[N, X, Configs, R] = new AddQueens[N, X, Configs] {
      override type Result = R
    }
  }

  trait AddQueensIf[P <: Bool, N <: Nat, X <: Nat, Configs <: HList] {
    type Result <: HList
  }

  object AddQueensIf {
    def apply[P <: Bool, N <: Nat, X <: Nat, Configs <: HList](
                                                                implicit
                                                                a: AddQueensIf[P, N, X, Configs]): Aux[P, N, X, Configs, a.Result] = a

    type Aux[P <: Bool, N <: Nat, X <: Nat, Configs <: HList, R <: HList] =
      AddQueensIf[P, N, X, Configs] {type Result = R}

    implicit def a0[N <: Nat, X <: Nat, Configs <: HList]
    : Aux[False, N, X, Configs, Configs] = new AddQueensIf[False, N, X, Configs] {
      override type Result = Configs
    }

    implicit def a1[N <: Nat,
      X <: Nat,
      Configs <: HList,
      Configs2 <: HList,
      R <: HList](implicit
                  aqa: AddQueenToAll.Aux[N, X, Configs, Configs2],
                  aq: AddQueens.Aux[N, Succ[X], Configs2, R])
    : Aux[True, N, X, Configs, R] = new AddQueensIf[True, N, X, Configs] {
      override type Result = R
    }
  }

  trait Solution[N <: Nat] {
    type Result <: HList
  }

  object Solution {
    def apply[N <: Nat](implicit s: Solution[N]): Aux[N, s.Result] = s

    type Aux[N <: Nat, R <: HList] = Solution[N] {type Result = R}

    implicit def s0[N <: Nat, Configs <: HList](
                                                 implicit
                                                 aq: AddQueens.Aux[N, _0, HNil :: HNil, Configs]): Aux[N, Configs] = new Solution[N] {
      override type Result = Configs
    }
  }

}
