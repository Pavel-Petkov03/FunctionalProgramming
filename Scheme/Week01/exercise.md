# Документация
[R5RS Manual](https://schemers.org/Documents/Standards/R5RS/HTML/)
[Racket reference](https://docs.racket-lang.org/reference/)

Инсталирайте [Dr Racket](https://racket-lang.org/)

## Задачи за проста аритметика

- (1) Преведете следните аритметични изрази на Scheme. Ако не знаете името
на функцията, смятаща някоя от аритметичните операции, използвайте
документацията
```
(3 + 5)/2 + √(4² - 7*2²)
(5 + 1/4 + (2 - (3 - (6 + 1/5)))) / 3(6 - 2)(2 - 7)
(15 + 21 + (3 / 15) + (7 - (2 * 2))) / 16
```

- (2) Двете булеви стойности в Scheme са `#t` и `#f`. Знаейки това, имплементирайте следните
булеви функции:
    - (my-not x)
    - (my-and x y)
    - (my-or x y)
    - (my-xor x y)

- (3) Имплементирайте функция `(fact n)`, която намира `n!`
- (4) Имплементирайте функция `(fib n)`, която намира n-тото число на Фибоначи
- (5) Напишете функция `(count-digits n)`, която смята броя на цифрите на дадено естествено число.
- (6) Напишете функция (palindrome? base n), която проверява дали дадено естествено число `n` е палиндром в бройната система с база `base`

## Итерация и опашкова рекурсия
- (7) Имплементирайте функцията `(fact n)`, така че да заема O(1) памет
- (8) Имплементирайте функцията `(fib n)`, така че да заема O(1) памет

## Задачи за [естествени числа на Пеано](https://en.wikipedia.org/wiki/Peano_axioms):
- (9) Дефинирайте функцията `(succ n)`, която за дадено естествено число n намира n+1, използвайки вградената функция `+`.

За следните задачи, не използвайте никакви аритметични операции (`+`, `-`, `*`, `%` и т.н.); Можете да използвате функции, които вече сте дефинирали.
- (10) `(pred n)`, която за дадено естествено число n намира n-1
- (11) `(add a b)`, която намира сумата на 2 естествени числа
- (12) `(multiply a b)`, която намира произведението на 2 естествени числа
- (13) `(fact n)`, която намира `n!`
- (14) `(safe-div n)` която за дадено число n, връща `n/2` ако `n` е четно, и `(n - 1)/2` в противен случай
- (15) `(fib n)`, която намира n-тото число на Фибоначи
- (15) (*) `(ack n a b)`, която изпълнява аритметична операция от ред n върху две числа. Приемаме, че add e от ред 1, multiply е от ред 2 и т.н.

![jedi](https://imgs.xkcd.com/comics/lisp_cycles.png)