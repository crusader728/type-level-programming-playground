import playground.NQueens._

implicitly[First.Aux[_1 :: HNil, _1]]

implicitly[Concat.Aux[_1 :: HNil, HNil, _1 :: HNil]]
implicitly[
  Concat.Aux[
    _1 :: _2 :: HNil,
    _3 :: _4 :: HNil,
    (_1 :: _2 :: _3 :: _4 :: HNil)
  ]
]

private type A = _1 :: _2 :: HNil
private type B = _3 :: _4 :: HNil
private type L = A :: B :: HNil
private type C = _1 :: _2 :: _3 :: _4 :: HNil
implicitly[Flatten.Aux[L, C]]

implicitly[AnyTrue.Aux[True :: HNil, True]]
implicitly[AnyTrue.Aux[False :: True :: HNil, True]]
implicitly[AnyTrue.Aux[False :: False :: HNil, False]]

implicitly[Or.Aux[True, False, True]]
implicitly[Or.Aux[True, False, True]]

implicitly[Eq.Aux[_1, _1, True]]
implicitly[Eq.Aux[_1, _2, False]]

implicitly[Lt.Aux[_2, _2, False]]
implicitly[Lt.Aux[_4, _5, True]]
implicitly[Lt.Aux[_6, _5, False]]

implicitly[AbsDiff.Aux[_3, _5, _2]]
implicitly[AbsDiff.Aux[_1, _6, _5]]

implicitly[QueensInRow.Aux[_2, _0, Queen[_0, _1] :: Queen[_0, _0] :: HNil]]

implicitly[Threats.Aux[Queen[_0, _0], Queen[_10, _9], False]]
implicitly[Threats.Aux[Queen[_0, _0], Queen[_1, _9], False]]
implicitly[Threats.Aux[Queen[_0, _0], Queen[_0, _9], True]]
implicitly[Threats.Aux[Queen[_0, _0], Queen[_9, _0], True]]
implicitly[Threats.Aux[Queen[_0, _0], Queen[_10, _10], True]]

implicitly[Safe.Aux[HNil, Queen[_1, _2], True]]
implicitly[Safe.Aux[Queen[_0, _0] :: HNil, Queen[_0, _2], False]]
implicitly[Safe.Aux[Queen[_0, _0] :: HNil, Queen[_1, _2], True]]
implicitly[Safe.Aux[Queen[_0, _0] :: Queen[_1, _2] :: Queen[_3, _5] :: HNil, Queen[_9, _6], True]]

implicitly[Apply.Aux[QueenX[_0], _3, Queen[_0, _3]]]

typeInfo(AddQueen.apply[_4, _1, Queen[_0, _0] :: HNil])

typeInfo(FlattenMap.apply[Conj[_1 :: HNil], _2 :: _3 :: HNil])

typeInfo(AddQueenToAll.apply[_4, _1, HNil])

typeInfo(AddQueenToAll.apply[_4, _1, (Queen[_0, _0] :: HNil) :: HNil])

typeInfo(AddQueensIf.apply[False, _10, _7, Queen[_0, _0] :: HNil])

typeInfo(AddQueensIf.apply[True, _4, _2, Queen[_0, _0] :: HNil])