#lang play
(require "interp-service.rkt")


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
;return a list (with nil symbols) and with size defined by ast's height
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

;get-left-child-index :: int -> int
;return left child index
(define (get-left-child-index index)
  (+ (* 2 index) 1))

;get-rigth-child-index :: int -> int
;return right child index
(define (get-right-child-index index)
  (+ (* 2 index) 2))

;set-ast-values :: ast list int-> list
;replace (mutation) values from ast in the list (heap binary complete ast representation)
(define (set-ast-values ast lst current-index-ast)
  (match ast
      [(num n) (list-set lst current-index-ast n)]
      [(add l r) (begin
                   (def l-lst (set-ast-values l lst (get-left-child-index current-index-ast)))
                   (def r-lst (set-ast-values r l-lst (get-right-child-index current-index-ast)))
                   (list-set r-lst current-index-ast '+))]
      [(sub l r) (begin
                   (def l-lst (set-ast-values l lst (get-left-child-index current-index-ast)))
                   (def r-lst (set-ast-values r l-lst (get-right-child-index current-index-ast)))
                   (list-set r-lst current-index-ast '-))]
      [(mult l r) (begin
                   (def l-lst (set-ast-values l lst (get-left-child-index current-index-ast)))
                   (def r-lst (set-ast-values r l-lst (get-right-child-index current-index-ast)))
                   (list-set r-lst current-index-ast '*))]
      [(div l r) (begin
                   (def l-lst (set-ast-values l lst (get-left-child-index current-index-ast)))
                   (def r-lst (set-ast-values r l-lst (get-right-child-index current-index-ast)))
                   (list-set r-lst current-index-ast '/))])
  )

(test (set-ast-values (num 1) '(nil) 0)
      '(1))

(test (set-ast-values (add (num 1) (num 5)) '(nil nil nil)  0)
      '(+ 1 5))

(test (set-ast-values (mult (num 1) (add (num 5) (num 8))) '(nil nil nil nil nil nil nil)  0)
      '(* 1 + nil nil 5 8))

(test (set-ast-values (div (sub (num 5) (num 8)) (num 1)) '(nil nil nil nil nil nil nil)  0)
      '(/ - 1 5 8 nil nil))

;get-ast-value :: ast -> value (concrete sintax)
;return concrete value from ast node
(define (get-ast-value ast)
  (match ast
    [(num n) n]
    [(add _ _) '+]
    [(sub _ _) '-]
    [(mult _ _) '*]
    [(div _ _) '/]))

(test (get-ast-value (num 1))
      1)

(test (get-ast-value (add (num 1) (num 5)))
      '+)
#|
;complete-ast :: ast -> ast
;return a complete ast. In case when is already, nothing to do, but in case when is not complete with (empty-node)
(define (complete-ast ast)
  
  )
(test (complete-ast (add (num 1) (num 3)))
      (add (num 1) (num 3)))

(test (complete-ast (sub (add (num 1) (num 3)) (num 5)))
      (add (add (num 1) (num 3)) (num 5))
|#