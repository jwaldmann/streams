Note: this software is for experimentation with
concepts explained in "Degrees of Streams" by Endrullis et al.,
see http://joerg.endrullis.de/research.html#degrees

== Some D0L Words ==

```
streamfix x f = let s = x : tail ( s >>= f) in s

fib   = streamfix 0 $ \ case 0 -> [0,1]  ; 1 -> [0] 
thue  = streamfix 0 $ \ case 0 -> [0,1]  ; 1 -> [1,0]
morse = streamfix 0 $ \ case 0 -> [0,1,2]; 1 -> [0,2]; 2 -> [1]
pdbl  = streamfix 0 $ \ case 0 -> [0,1] ; 1 -> [0,0]
waltz = streamfix 0 $ \ case 0 -> [0,0,1]; 1 -> [1,1,0]
```
Names in the literature sometimes differ,
e.g., our `morse` is often called Thue-Morse, etc.

== Some FST Reductions ==

It is well-known that we can go from Morse to Thue via a morphism (that is, number of states is 1)
```
find-fst -s 1 -w 3 -c 1000 -f Morse -t Thue
input  : [0,1,2,0,2,1,0,1,2,1,0,2,0,1,2,0,2,1,0,2]
output : [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1]

FST A (fromList [((A,0),([0,1,1],A)),((A,1),([0,1],A)),((A,2),([0],A))])

```
It is interesting that we can also go back
but we need 3 states.
```
find-fst -s 3 -w 1 -c 1000 -t Morse -f Thue
input  : [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1]
output : [0,1,2,0,2,1,0,1,2,1,0,2,0,1,2,0,2,1,0,2]

FST A (fromList [((A,0),([],A)),((A,1),([],B)),((B,0),([],C)),((B,1),([0],B)),((C,0),([2],A)),((C,1),([1],B))])

```
One should always check this with larger values for `-c`,
and finally, on paper.

So, Thue and Morse are FTS-equivalent.

We have these stream operators (which are actually FSTs)
```
-- | substream: every second letter
second (x:y:rest) = x: second rest

-- | substream: every third letter
third (x:y:z:rest) = x: third rest
```
and we can use the program to discover the FST,
which is the second solution printed below.
It is an exercise to verify that the first solution is correct.
```
find-fst -s 2 -w 1 -c 10000 -f Fib -t 'Snd Fib'
input  : [0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1]
output : [0,0,1,1,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0,1]

FST A (fromList [((A,0),([0],A)),((A,1),([0],B)),((B,0),([1],B)),((B,1),([],A))])
FST A (fromList [((A,0),([0],B)),((A,1),([1],B)),((B,0),([],A)),((B,1),([],A))])

```
We find an FST that reconstructs the original sequence
```
find-fst -s 2 -w 3 -c 100000 -f 'Snd Fib' -t Fib
input  : [0,0,1,1,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0,1]
output : [0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1]

FST A (fromList [((A,0),([0],B)),((A,1),([1,0],A)),((B,0),([1,0],B)),((B,1),([0,1,0],A))])

```
This means that `Fib` and `Snd Fib` are FST-equivalent.
It is open whether `Thrd Fib`  is in the same class.

The paper also states that Waltz and Sierpinski are FST-equivalent. Let us check this. From Sierpinski to Waltz:
```
find-fst -s 4 -w 2 -c 10000 -f Sierp -t Waltz -l 1
input  : [0,0,1,1,1,1,0,0,0,1,1,0,0,0,0,1,1,0,0,0]
output : [0,0,1,0,0,1,1,1,0,0,0,1,0,0,1,1,1,0,1,1]

FST A (fromList [((A,0),([],B)),((A,1),([],B)),((B,0),([0],A)),((B,1),([0],C)),((C,0),([],D)),((C,1),([],D)),((D,0),([1],C)),((D,1),([1],A))])

```
Can we go back? The paper mentions the delta operator
(and `delta $ delta $ delta waltz == delta $ delta sierp`)
so we try
```
find-fst -s 2 -w 1 -c 10000 -f 'Delta Waltz' -t Sierp -l 1
input  : [0,1,1,0,1,0,0,1,0,0,1,1,0,1,0,0,1,1,0,1]
output : [0,0,1,1,1,1,0,0,0,1,1,0,0,0,0,1,1,0,0,0]

FST A (fromList [((A,0),([0],B)),((A,1),([1],B)),((B,0),([1],A)),((B,1),([0],A))])
```
indeed these two streams are related by a difference
in every other position. An FST is able to compute delta,
and then we just compose it with "change every other letter".
How many states would the composed FST have?
Perhaps the produce of the number of states of the factors,
that is, 2 by 2 = 4? Then the following should work,
but doesn't. Why?
```
find-fst -s 4 -w 1 -c 1000 -f Waltz -t Sierp -l 1
```

