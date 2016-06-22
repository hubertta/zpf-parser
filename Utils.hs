module Utils where

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f s =
    let ff = f s in case ff == s of
        True -> ff
        False -> fixpoint f ff
