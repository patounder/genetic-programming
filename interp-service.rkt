#lang play

(print-only-errors #t)
#|
<s-expr> :: <number>
          | {+ <s-expr> <s-expr>}
          | {- <s-expr> <s-expr>}
          | {* <s-expr> <s-expr>}
|#


;Se usa una estructura de datos para representar las expresiones posibles en el lenguaje definido

(deftype expr
  (num n)
  (add l r)
  (sub l r)
  (mult l r)
  )


#|
Estructura de datos que permitira representar representar el tipo de dato de una expresion

type : (TNum)
|#

(deftype Type
  (TNum)
  )

#|
parse :: s-expr -> expr

dada una s-expr retorna el respectivo ast con expr
|#

(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
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
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(mult l r) (* (interp l) (interp r))])
  )

(test (interp (parse 4))
      4)

(test (interp (parse '{+ 5 {- 3 0}}))
      8)

(test (interp (parse '[- (+ 3 1) 10]))
      -6)

(test (interp (parse '[* (+ 3 1) 10]))
      40)

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
    [(add l r)
     (check-type (TNum) (typeof l) 1)
     (check-type (TNum) (typeof r) 2)
     (TNum)
     ]
    [(sub l r)
     (check-type (TNum) (typeof l) 1)
     (check-type (TNum) (typeof r) 2)
     (TNum)]
    [(mult l r)
     (check-type (TNum) (typeof l) 1)
     (check-type (TNum) (typeof r) 2)
     (TNum)]
    )
  )

(test
 (typeof (parse '{+ 3 1}))
 (TNum)
 )