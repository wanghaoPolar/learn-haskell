> module FoldTraining where

> import Prelude hiding (foldr, foldl)

1 Foldr
Say we want to calculate the sum of a very big list:

> veryBigList = [1..1000000]

Lets start with the following:

> foldr f z []     = z
> foldr f z (x:xs) = x `f` foldr f z xs

> sum1 = foldr (+) 0

> try1 = sum1 veryBigList

If we evaluate try1 we get:

*** Exception: stack overflow

Too bad... So what happened:

try1 -->
sum1 veryBigList -->
foldr (+) 0 veryBigList -->

foldr (+) 0 [1..1000000] -->
1 + (foldr (+) 0 [2..1000000]) -->
1 + (2 + (foldr (+) 0 [3..1000000])) -->
1 + (2 + (3 + (foldr (+) 0 [4..1000000]))) -->
1 + (2 + (3 + (4 + (foldr (+) 0 [5..1000000])))) -->
-- ...
-- ...  My stack overflows when there's a chain of around 500000 (+)'s !!!
-- ...  But the following would happen if you got a large enough stack:
-- ...
1 + (2 + (3 + (4 + (... + (999999 + (foldr (+) 0 [1000000]))...)))) -->
1 + (2 + (3 + (4 + (... + (999999 + (1000000 + ((foldr (+) 0 []))))...)))) -->

1 + (2 + (3 + (4 + (... + (999999 + (1000000 + 0))...)))) -->
1 + (2 + (3 + (4 + (... + (999999 + 1000000)...)))) -->
1 + (2 + (3 + (4 + (... + 1999999 ...)))) -->

1 + (2 + (3 + (4 + 500000499990))) -->
1 + (2 + (3 + 500000499994)) -->
1 + (2 + 500000499997) -->
1 + 500000499999 -->
500000500000
The problem is that (+) is strict in both of its arguments.
This means that both arguments must be fully evaluated before (+)
can return a result. So to evaluate:

1 + (2 + (3 + (4 + (...))))
1 is pushed on the stack. Then:

2 + (3 + (4 + (...)))
is evaluated. So 2 is pushed on the stack. Then:

3 + (4 + (...))
is evaluated. So 3 is pushed on the stack. Then:

4 + (...)
is evaluated. So 4 is pushed on the stack. Then: ...

... your limited stack will eventually run full
when you evaluate a large enough chain of (+)s.
This then triggers the stack overflow exception.

Lets think about how to solve it...

2 Foldl
One problem with the chain of (+)'s is that it can't be made smaller
(reduced) until the very last moment, when it's already too late.

The reason we can't reduce it is that the chain doesn't contain
an expression which can be reduced
(a redex, for reducible expression.)
If it did we could reduce that expression before going to the next element.

We can introduce a redex by forming the chain in another way.
If instead of the chain 1 + (2 + (3 + (...)))
we could form the chain (((0 + 1) + 2) + 3) + ..., then there would always be a redex.

We can form such a chain by using a function called foldl:

> foldl f z []     = z
> foldl f z (x:xs) = let z' = z `f` x
>                    in foldl f z' xs

> sum2 = foldl (+) 0

> try2 = sum2 veryBigList

Lets evaluate try2:

*** Exception: stack overflow

Good Lord! Again a stack overflow! Lets see what happens:

try2 -->
sum2 veryBigList -->
foldl (+) 0 veryBigList -->

foldl (+) 0 [1..1000000] -->

let z1 =  0 + 1
in foldl (+) z1 [2..1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
in foldl (+) z2 [3..1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
in foldl (+) z3 [4..1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
in foldl (+) z4 [5..1000000] -->

-- ... after many foldl steps ...

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
in foldl (+) z999997 [999998..1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
in foldl (+) z999998 [999999..1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
    z999999 = z999998 + 999999
in foldl (+) z999999 [1000000] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
    z999999 = z999998 + 999999
    z100000 = z999999 + 1000000
in foldl (+) z1000000 [] -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
    z999999 = z999998 + 999999
    z100000 = z999999 + 1000000
in z1000000 -->

-- Now a large chain of +'s will be created:

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
    z999999 = z999998 + 999999
in z999999 + 1000000 -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
in  (z999998 + 999999) + 1000000 -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
in  ((z999997 + 999998) + 999999) + 1000000 -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
in  (((z999996 + 999997) + 999998) + 999999) + 1000000 -->

-- ...
-- ... My stack overflows when there's a chain of around 500000 (+)'s !!!
-- ... But the following would happen if you got a large enough stack:
-- ...

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
in  (((((z4 + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
in  ((((((z3 + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

let z1 =  0 + 1
    z2 = z1 + 2
in  (((((((z2 + 3) + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

let z1 =  0 + 1
in  ((((((((z1 + 2) + 3) + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

(((((((((0 + 1) + 2) + 3) + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

-- Now we can actually start reducing:

((((((((1 + 2) + 3) + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

(((((((3 + 3) + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

((((((6 + 4) + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

(((((10 + 5) + ...) + 999997) + 999998) + 999999) + 1000000 -->

((((15 + ...) + 999997) + 999998) + 999999) + 1000000 -->

(((499996500006 + 999997) + 999998) + 999999) + 1000000 -->

((499997500003 + 999998) + 999999) + 1000000 -->

(499998500001 + 999999) + 1000000 -->

499999500000 + 1000000 -->

500000500000 -->
Well, you clearly see that the redexes are created.
But instead of being directly reduced, they are allocated on the heap:

let z1 =  0 + 1
    z2 = z1 + 2
    z3 = z2 + 3
    z4 = z3 + 4
    ...
    z999997 = z999996 + 999997
    z999998 = z999997 + 999998
    z999999 = z999998 + 999999
    z1000000 = z999999 + 1000000
in z1000000
Note that your heap is only limited by the amount of memory in your system (RAM and swap).
So the only thing this does is filling up a large part of your memory.

The problem starts when we finally evaluate z1000000:

We must evaluate z1000000 = z999999 + 1000000,
so 1000000 is pushed on the stack.
Then z999999 is evaluated; z999999 = z999998 + 999999,
so 999999 is pushed on the stack.
Then z999998 is evaluated; z999998 = z999997 + 999998,
so 999998 is pushed on the stack.
Then z999997 is evaluated...

...your stack will eventually fill when you evaluate a large enough chain of (+)'s.
This then triggers the stack overflow exception.

But this is exactly the problem we had in the foldr case —
only now the chain of (+)'s is going to the left instead of the right.

So why doesn't the chain reduce sooner than before?

It's because of GHC's lazy reduction strategy:
expressions are reduced only when they are actually needed.
In this case, the outer-left-most redexes are reduced first.
In this case it's the outer foldl (+) ... [1..10000] redexes which are repeatedly reduced.
So the inner z1, z2, z3, ... redexes only get reduced when the foldl is completely gone.

3 Foldl'
We somehow have to tell the system that the inner redex should be reduced before the outer.
Fortunately this is possible with the seq function:

seq :: a -> b -> b
seq is a primitive system function that when applied to x and y will first reduce x
then return y.
The idea is that y references x so that when y is reduced x
will not be a big unreduced chain anymore.

Now lets fill in the pieces:

> foldl' f z []     = z
> foldl' f z (x:xs) = let z' = z `f` x
>                     in seq z' $ foldl' f z' xs

> sum3 = foldl' (+) 0

> try3 = sum3 veryBigList

If we now evaluate try3 we get the correct answer and we get it very quickly:

500000500000
Lets see what happens:

try3 -->
sum3 veryBigList -->
foldl' (+) 0 veryBigList -->

foldl' (+) 0 [1..1000000] -->
foldl' (+) 1 [2..1000000] -->
foldl' (+) 3 [3..1000000] -->
foldl' (+) 6 [4..1000000] -->
foldl' (+) 10 [5..1000000] -->
-- ...
-- ... You see that the stack doesn't overflow
-- ...
foldl' (+) 499999500000 [1000000] -->
foldl' (+) 500000500000 [] -->
500000500000
You can clearly see that the inner redex is repeatedly reduced first.

4 Conclusion
Usually the choice is between foldr and foldl',
since foldl and foldl' are the same except for their strictness properties,
so if both return a result, it must be the same.
foldl' is the more efficient way to arrive at that result
because it doesn't build a huge thunk.
However, if the combining function is lazy in its first argument,
foldl may happily return a result where foldl' hits an exception:

> (?) :: Int -> Int -> Int
> _ ? 0 = 0
> x ? y = x*y
>
> list :: [Int]
> list = [2,3,undefined,5,0]
>
> okey = foldl (?) 1 list
>
> boom = foldl' (?) 1 list

Let's see what happens:

okey -->
foldl (?) 1 [2,3,undefined,5,0] -->
foldl (?) (1 ? 2) [3,undefined,5,0] -->
foldl (?) ((1 ? 2) ? 3) [undefined,5,0] -->
foldl (?) (((1 ? 2) ? 3) ? undefined) [5,0] -->
foldl (?) ((((1 ? 2) ? 3) ? undefined) ? 5) [0] -->
foldl (?) (((((1 ? 2) ? 3) ? undefined) ? 5) ? 0) [] -->
((((1 ? 2) ? 3) ? undefined) ? 5) ? 0 -->
0

boom -->
foldl' (?) 1 [2,3,undefined,5,0] -->
    1 ? 2 --> 2
foldl' (?) 2 [3,undefined,5,0] -->
    2 ? 3 --> 6
foldl' (?) 6 [undefined,5,0] -->
    6 ? undefined -->
*** Exception: Prelude.undefined
Note that even foldl' may not do what you expect.
The involved seq function does only evaluate the top-most constructor.
If the accumulator is a more complex object,
then fold' will still build up unevaluated thunks.
You can introduce a function or a strict data type which forces the values
as far as you need.
Failing that, the "brute force" solution is to use deepseq.
For a worked example of this issue, see Real World Haskell chapter 25.

5 Rules of Thumb for Folds
Folds are among the most useful and common functions in Haskell.
They are an often-superior replacement for what in other language would be loops,
but can do much more. Here are a few rules of thumb on which folds to use when.

foldr is not only the right fold, it is also most commonly the right fold to use,
in particular when transforming lists (or other foldables) into lists
with related elements in the same order.
Notably, foldr will be effective for transforming even infinite lists
into other infinite lists.
For such purposes, it should be your first and most natural choice.
For example, note that foldr (:) []==id.

Note that the initial element is irrelevant
when foldr is applied to an infinite list.
For that reason,
it is may be good practice when writing a function which should only be applied to
infinite lists to replace foldr f [] with foldr f undefined.
This both documents that the function should only be applied to infinite lists
and will result in an error when you try to apply it to a finite list.

The other very useful fold is foldl'.
It can be thought of as a foldr with these differences:
foldl' conceptually reverses the order of the list.
One consequence is that a foldl' (unlike foldr) applied to an infinite list will be bottom;
it will not produce any usable results, just as an express reverse would not.
Note that foldl' (flip (:)) []==reverse.
foldl' often has much better time and space performance than
a foldr would for the reasons explained in the previous sections.
You should pick foldl' principally in two cases:
When the list to which it is applied is large,
but definitely finite,
you do not care about the implicit reversal
(for example, because your combining function is commutative like (+), (*), or Set.union),
and you seek to improve the performance of your code.
When you actually do want to reverse the order of the list,
in addition to possibly performing some other transformation to the elements.
In particular, if you find that you precede or follow your fold with a reverse,
it is quite likely that you could improve your code by using the other fold and
taking advantage of the implicit reverse.
foldl is rarely the right choice.
It gives you the implicit reverse of fold,
but without the performance gains of foldl'.
Only in rare, or specially constructed cases like in the previous section,
will it yield better results than foldl'.

Another reason that foldr is often the better choice is that
the folding function can short-circuit, that is,
terminate early by yielding a result which does not depend on the value
of the accumulating parameter.
When such possibilities arise with some frequency in your problem,
short-circuiting can greatly improve your program's performance.
Left folds can never short-circuit.

To illustrate this consider writing a fold that computes the product of
the last digits of a list of integers.
One might think that foldl' is the superior fold in this situation
as the result does not depend on the order of the list and
is generally not computable on infinite lists anyway.

On my workstation running GHC 7.10.2,
foldl' (\a e -> (mod e 10)*a) 1 [1..10^7]
has a compiled run-time of 422 ms (all of it calculation)
and allocates over 400 MBytes on the heap.
Conversely, foldr (\e a -> (mod e 10)*a) 1 [1..10^7]
has a compiled run-time of 2203 ms
(the same 422 ms calculation time and 1781 ms of garbage collection)
and allocates more than 550 MBytes on the heap.
That is what you should expect from reading the previous sections;
foldr will build up a huge stack of nested expression
before evaluating the result and that stack needs to be garbage collected.

But think about the problem a little more.
Once the fold hits a number with last digit 0,
there is no need to evaluate any further.
The ultimate result will always be 0,
so you can short-circuit to that answer immediately.
Indeed foldr (\e a -> if mod e 10==0 then 0 else (mod e 10)*a) 1 [1..10^7]
has a measured run-time of 0ms and allocates less than 50 kBytes on the heap.
This fold will run just as fast on the range [1..10^100] or even [1..].

The left fold cannot short-circuit and is condemned to evaluate the entire input list.
Running foldl' (\a e -> if mod e 10==0 then 0 else (mod e 10)*a) 1 [1..10^7]
takes 781 ms and allocates over 500 MByte of heap space;
it is inferior to even the original left fold,
not to mention the short-circuiting right fold.
