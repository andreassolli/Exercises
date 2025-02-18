data NaturalNumber = Zero | Successor NaturalNumber
    deriving (Eq, Show, Read, Ord)

addNat :: NaturalNumber -> NaturalNumber -> NaturalNumber
addNat Zero x = x
addNat (Successor y) x = Successor (addNat x y)

mulNat :: NaturalNumber -> NaturalNumber -> NaturalNumber
mulNat Zero x = Zero
mulNat (Successor (Successor y)) x = addNat x (addNat x y)


nat2int :: NaturalNumber -> Int
nat2int Zero = 0
nat2int (Successor n) = 1 + nat2int n

int2nat :: Int -> NaturalNumber
int2nat 0 = Zero
int2nat n = Successor (int2nat (n - 1))

main :: IO ()
main = do
    let nat = Successor (Successor Zero)        
        int = 3
        nat2 = Successor (Successor (Successor (Successor Zero)))
    print (nat2int nat)
    print (nat2int nat2)
    print (int2nat int)
    print (addNat nat nat2)
    print (mulNat nat nat2)
    print ("Multiplication: " ++ show(nat2int (mulNat nat nat2)))
