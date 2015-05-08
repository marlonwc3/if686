1ª)
foldr ((+).((.).map)

(.).map 
(.) :: (b->c)->(a->b)->(a->c)
(.) :: (e->f)->(d->e)->(d->f)
map :: (g -> h) -> [g] -> [h]

d == (g->h)
e == [g] -> [h] 

e == b->c
f == (a->b) -> (a->c)

b == [g] 
c == [h] 

f == (a->[g]) -> (a -> [h])

(.).map :: (g->h) -> (a -> [g]) -> (a -> [h]) 

(+) :: Num i => i -> i -> i
. :: (k->l) -> (j->k) -> (j -> l)
(.).map :: (g->h) -> (a -> [g]) -> (a -> [h]) 

j == g->h
k ==  (a -> [g]) -> (a -> [h]) 

k == Num i => i
l == Num i => i -> i

i ==  (a -> [g]) -> (a -> [h]) 

l == ((a -> [g]) -> a -> [h] ) -> ( (a -> [g]) -> a -> [h] )

(+).(.).map :: Num ((a -> [g]) -> a -> [h])  => (g->h) -> ((a -> [g]) -> a -> [h] ) -> ( (a -> [g]) -> a -> [h] )


foldr :: (n->m->m) -> m -> [n] -> m
(+).(.).map :: Num ((a -> [g]) -> a -> [h])  => (g->h) -> ((a -> [g]) -> a -> [h] ) -> ( (a -> [g]) -> a -> [h] )


n ==  (g->h)
m == ((a -> [g]) -> a -> [h] )

foldr (+).(.).map :: ((a -> [g]) -> a -> [h] ) -> [(g->h)] -> ((a -> [g]) -> a -> [h] )

     
2ª) 
(\x y z -> foldr z x y) . map

foldr :: (b->a->a) -> a -> [b] -> a
\x y z -> foldr z x y :: x -> y -> z -> a
z == (b->a->a) 
x == a
y == [b]

\x y z -> foldr z x y :: a -> [b] -> (b->a->a) -> a 
(.) :: (d->e)->(c->d)->(c->e)
map :: (f->g) -> [f] -> [g]

c == (f->g)
d == [f] -> [g]

d == a 
e == ([b] -> (b ->a -> a) -> a) 

a == [f] -> [g] 
e == ([b] -> (b -> ([f] -> [g]) -> ([f] -> [g]) ) -> [f] -> [g] ) 

(\x y z -> foldr z x y) . map :: (f->g) -> [b] -> (b -> ([f] -> [g]) -> ([f] -> [g]) ) -> [f] -> [g] 


3ª) 
map.((.) (foldr (++) (foldr (++) [] [[1], [2]])))
foldr (++) [] [[1], [2]]
== [] ++ [2]
== [1] ++ [2] 
= [1,2] :: Num a => [a] 

foldr (++) [1,2] :: 
foldr :: (c->b->b)->b->[c]->b
(++) :: [d] -> [d] -> [d]
[1,2] :: Num a => [a] 

c == [d]
b == [d]
b == [d] 


b = Num a => [a]
a = d
c = Num a => [a]
b = Num a => [a]
(foldr (++) (foldr (++) [] [[1], [2]]))) :: Num a => [[a]] -> [a]
(.) :: (f->g)->(e->f)->(e->g)
f == Num a => [[a]]
g == NUm a => [a] 
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: Num a => (e -> [[a]])->(e->[a])

map :: (h -> i) -> [h] -> [i] 
(.) :: (k->l)->(j->k)->(j->l)
(.) (foldr (++) (foldr (++) [] [[1], [2]])) :: Num a => (e -> [[a]])->(e->[a])

j == e -> [[a]]
k == e->[a]

k == (h -> i)
l == [h] -> [i] 

e == h
i == [a]
l == Num a => [h] ->[[a]]
j == NUm a => h -> [[a]]
map.((.) (foldr (++) (foldr (++) [] [[1], [2]]))) :: Num a => (h -> [[a]]) -> h -> [[a]]



4ª ) 

(foldr).(.)$(!!)

foldr :: (b->a->a) -> a -> [b] -> a 
(.) :: (d->e)->(c->d)->(c->e)
(.) :: (g->h)->(f->g)->(f->h)
c == (g->h) 
d = (f->g)->f->h

d =  (b->a->a)
e = a -> [b] -> a

b = (f->g)
f = a 
h = a

b = (a->g)
c = (g->a)
e = a -> [(a->g] ->a

(foldr).(.) :: (g->a) -> a -> [a->g] ->a
($) ::  (i->j) -> i -> j
(!!) :: [k] -> Int -> k

i = (g->a) 
j = a -> [a->g] -> a

i = [k] -> Int -> k 

g = [k] 
a = Int -> k

j = (Int -> k) -> [ (Int -> k) -> [k] ] -> Int -> k 
(foldr).(.) $ (!!) :: (Int -> k) -> [ (Int -> k) -> [k] ] -> Int -> k  




















