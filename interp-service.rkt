#lang play
(print-only-errors #t)

;MACROS
(defmac (my-while cond body)
  (letrec ([iter (λ()
                   (if cond
                       (begin
                         body
                         (iter))
                       (void)))])
    (iter)))


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

;get-heigth :: ast -> int
;return ast's height
(define (get-height ast)
  (match ast
    [(num n) 0]
    [(add l r) (+ 1 (max (get-height l) (get-height r)))]
    [(sub l r) (+ 1 (max (get-height l) (get-height r)))]
    [(mult l r) (+ 1 (max (get-height l) (get-height r)))]
    )
  )

(test (get-height (num 1))
      0)

(test (get-height (add (num 1) (num 2)))
      1)

(test (get-height (add (num 1) (sub (num 3) (num 4))))
      2)

(test (get-height (add (add (num 1) (mult (num 5) (add (num 4) (num 2)))) (sub (num 3) (num 4))))
      4)

;get-nil-ast-height :: ast -> list
;return a list (with nil symbols) with size of ast's height
(define (get-nil-ast-height ast)

  ;build-nil-list :: int list -> list
  (define (build-nil-list size lst)
    (if (equal? size 0)
        lst
        (build-nil-list (sub1 size) (cons 'nil lst))))

  (let ([height (get-height ast)]
        [i 0]
        [k 0]
        [size 0])
    (begin
      (my-while (<= i height)
                (begin
                  (set! size (+ size (expt 2 i)))
                  (set! i (add1 i))))
      (build-nil-list size '()))
    )
  )

(test (get-nil-ast-height (add (num 1) (num 2)))
      '(nil nil nil))

(test (get-nil-ast-height (add (num 1) (sub (num 3) (num 4))))
      '(nil nil nil nil nil nil nil))

(test (get-nil-ast-height (add (add (num 1) (mult (num 5) (add (num 4) (num 2)))) (sub (num 3) (num 4))))
      '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

#|
;complete-ast :: ast -> ast
;return a complete ast. In case when is already, nothing to do, but in case when is not complete with (empty-node)
(define (complete-ast ast)
  
  )
(test (complete-ast (add (num 1) (num 3)))
      (add (num 1) (num 3)))

(test (complete-ast (sub (add (num 1) (num 3)) (num 5)))
      (add (add (num 1) (num 3)) (num 5))

;ast-to-list :: ast -> list
;return ast (program) like list
(define (ast-to-list ast prog)
  (match ast
    [(num n) (list n)]
    [(add l r) (append prog (list '+ (ast-to-list l prog) ()))]
  ))

(test (ast-to-list (add (num 1) (num 3)))
      '(+ 1 3))

(test (ast-to-list (sub (add (num 4) (num 9)) (num 5)))
      '(- + 5 4 9 null null))

|#