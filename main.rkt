#lang play

(print-only-errors #t)

#|TODO'S

1 Randomly create an initial population of programs from the available primitives. Initial and futures. 
1.1 set data structure for support (attributes member, depth)
repeat
 2 Execute each program and ascertain its fitness. Not compile, compile but result not ok, result ok. Use try catch. Count errors by step lexic, semantic, and so.
 3 Select one or two program(s) from the population with a probability based on fitness to participate in genetic operations (Section 2.3).
 4 Create new individual program(s) by applying genetic operations with specified probabilities (Section 2.4).
   4.1 Crossover
   4.2 Mutation
until an acceptable solution is found or some other stopping condition is met (e.g., a maximum number of generations is reached).

return the best-so-far individual.

|#


#|
<s-expr> :: <number>
          | {+ <s-expr> <s-expr>}
          | {- <s-expr> <s-expr>}
|#


;Se usa una estructura de datos para representar las expresiones posibles en el lenguaje definido

(deftype expr
  (num n)
  (bool b)
  (add l r)
  (sub l r)
  )


#|
Estructura de datos que permitira representar representar el tipo de dato de una expresion

type : (TNum)
|#

(deftype Type
  (TNum)
  (TBool)
  )


(deftype Tipe
  (Num)
  (Bool))

#|
parse :: s-expr -> expr

dada una s-expr retorna el respectivo ast con expr
|#

(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    )
  )

(test (parse 4)
      (num 4))

(test (parse '{+ 5 {- 3 0}})
      (add (num 5) (sub (num 3) (num 0))))

#|
interp :: expr -> number or boolean
dada una expresion, retorna su valor numerico o booleano
|#

(define (interp expr)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))])
  )

(test (interp (parse 4))
      4)

(test (interp (parse '{+ 5 {- 3 0}}))
      8)

(test (interp (parse '[- (+ 3 1) 10]))
      -6)

;typeof :: expr -> type
;funcion que dada una expresion retorna el tipo asociado
(define (typeof expr)
  
  (define (check-type expec-type expr-type position)
    (if (equal? expec-type expr-type)
        expec-type
        (error "Expected type ~v and expresion is ~v in position ~v" expec-type expr-type position)
        )
    )

  (match expr
    [(num n) (TNum)]
    [(bool b) (TBool)]
    [(add l r)
     (check-type (TNum) (typeof l) 1)
     (check-type (TNum) (typeof r) 2)
     (TNum)
     ]
    [(sub l r)
     (check-type (TNum) (typeof l) 1)
     (check-type (TNum) (typeof r) 2)
     (TNum)]
    )
  )

(test
 (typeof (parse '{+ 3 1}))
 (TNum)
 )

(test/exn
 (typeof (parse '{+ 1 #f}))
 "Expected type"
 )