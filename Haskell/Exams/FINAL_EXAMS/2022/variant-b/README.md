
# Functional Programming Exam

**Date:** 8th February 2023  
**Specialties:** Informatics, Computer Science, Stream 1, and Elective  
**Variant:** B

## Problem 1 (12 points)

### Task
Implement a function `primitive` that, given a positive integer `k`, generates an infinite stream of all numbers that can be represented as the product of exactly `k` prime numbers, in ascending order and without repetitions.

### Example
```haskell
primitive 3 → [8, 12, 18, 20, 27, 28, …]
```

### Bonus (5 points)
Express `primitive k` recursively using `primitive (k-1)` and the stream of all prime numbers.

---

## Problem 2 (20 points)

### Task
A nondeterministic finite automaton (NFA) `N` is defined as a tuple `(Q, q0, T, F)` where:
- `Q` is a finite set of states.
- `q0 ∈ Q` is the initial state.
- `T ⊆ Q × {a,b} × Q` is the transition relation.
- `F ⊆ Q` is the set of accepting states.

A word `c1c2...cn` is recognized by the automaton `N` if there exists a sequence of transitions:
```
(q0, c1, q1), (q1, c2, q2), …, (qn-1, cn, qn)
```
such that `qn ∈ F`.

Implement a function `detectedWords` that returns a finite list of all words recognized by the automaton. If the automaton recognizes an infinite set of words, return an appropriate result.

The word representation in Scheme is a list of single-letter symbols, and in Haskell, it is a string. The automaton representation is your choice.

### Hint
The language of the automaton is infinite if there is a path from its initial state to a final state that contains a cycle.

### Example
For the automaton:
```
Q = {0, 1, 2, 3}
q0 = 0
T = {(0,a,1),(1,b,1),(0,a,2),(0,b,2),(2,a,3)}
F = {0, 2, 3}
```
The recognized words are:
```
{"", "aa", "ab"}
```
If we add the transition `(1,a,2)`, the language becomes infinite.

---

## Problem 3 (12 points)

### Task
Let `f` be a list of two-argument numerical functions `f0, f1, …, fm-1`, and let `l` be a list of numbers `x0, x1, …, xn-1`. The cyclic application of `f` over `l` at position `i` is defined as:
```
f0(xi, f1(xi+1, …, fn-i-1(xn-1, fn-i(x0, fn-i+1(x1, …, fm-1(xj-1, xj)…))…)))
```
where `j` is the corresponding last index.

Implement a function `findMax` that, given a list of functions `f` and a list of numbers `l`, finds the maximum of the `n` possible cyclic applications of `f` over `l`.

### Example
```haskell
findMax [(+),(*),(-)] [2,5] → 11  -- (= 5 + (2 * (5 – 2)))
```

---

## Problem 4 (12 points)

### Task
A group of friends plays a board game in which they receive an integer number of points. Before the game starts, each friend tries to predict how many points they will score. After the game, the absolute value of the difference between their prediction and the actual score is calculated, which we call the error.

The players are ranked by the accuracy of their predictions (i.e., in ascending order of the error). The final score is determined as follows: the player with the k-th worst prediction error receives bonus points equal to the error of the player with the k-th best prediction.

Implement a function `finalScores` that, given a list of triples containing the player’s name (string), prediction (integer), and actual score (integer), returns a list of pairs of the player’s name and their final score.

### Example
```haskell
finalScores [("Dejan",10,14),("Eslin",5,8),("Maria",11,10),("Martin",1,6)]
→ [("Maria",15),("Eslin",12),("Dejan",17),("Martin",7)]
```
