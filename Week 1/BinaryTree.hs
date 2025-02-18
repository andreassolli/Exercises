data Tree = Leaf | Node Integer Tree Tree
    deriving (Eq, Show, Read, Ord)

insert :: Integer -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
    | x == y = Node y left right
    | x < y  = Node y (insert x left) right
    | x > y  = Node y left (insert x right)

data PTree a = PLeaf | PNode a (PTree a) (PTree a)
    deriving (Eq, Show, Read, Ord)

pinsert :: (Ord a) => a -> PTree a -> PTree a
pinsert n PLeaf = PNode n PLeaf PLeaf
pinsert n (PNode value left right)
    | n == value = PNode value left right
    | n < value  = PNode value (pinsert n left) right
    | n > value  = PNode value left (pinsert n right)


main :: IO ()
main = do
    let tree = Node 5 
                (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)) 
                (Node 7 Leaf (Node 8 Leaf Leaf))
        newTree = insert 6 tree
        intTree = pinsert 5 (pinsert 3 (pinsert 7 PLeaf))
        stringTree = pinsert "banana" (pinsert "apple" (pinsert "cherry" PLeaf))
    print newTree   
    print intTree
    print stringTree