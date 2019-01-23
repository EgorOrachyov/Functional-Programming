module FastList (
		Seq
	) where

	data ElementOrPair a = Element a | Pair (ElementOrPair a) (ElementOrPair a)
	data Seq a = Empty | Seq (ElementOrPair a) (Seq a)


