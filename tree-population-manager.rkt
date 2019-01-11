#lang play
(require "interp-service.rkt")


;CONSTANTS
(define NON_TERMINAL_SET '(+ - *))
(define TERMINAL_SET '(1 2 5 7 9 11))

;get-left-child-index :: int -> int
;return left child index
(define (get-left-child-index index)
  (+ (* 2 index) 1))

;get-rigth-child-index :: int -> int
;return right child index
(define (get-right-child-index index)
  (+ (* 2 index) 2))

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

;heigth-sub-tree-list list int int -> int
;return height given ast like list (official), subtree's root and init height.
(define (height-sub-tree-list ast-list subtree-root init-height)
  (let ([index-left-child (get-left-child-index subtree-root)]
        [index-right-child (get-right-child-index subtree-root)])
    (if (and (and (<= index-left-child (length ast-list))
                  (<= index-right-child (length ast-list)))
             (or (not (equal? (list-ref ast-list index-left-child) 'nil))
                 (not (equal? (list-ref ast-list index-right-child) 'nil))))
        (max (height-sub-tree-list ast-list index-left-child (add1 init-height))
             (height-sub-tree-list ast-list index-right-child (add1 init-height)))
        init-height)
    )
  )


(test (height-sub-tree-list '(* 11 + nil nil - 7 nil nil nil nil 7 1 nil nil) 2 0)
      2)

(test (height-sub-tree-list '(* 11 + nil nil 7 - nil nil nil nil nil nil 7 1) 2 0)
      2)

;build-nil-list :: int list -> list
(define (build-nil-list size lst)
  (if (equal? size 0)
      lst
      (build-nil-list (sub1 size) (cons 'nil lst))))

;get-nil-ast-height :: int -> list
;return a list (with nil symbols) and with size defined by ast's height
(define (get-nil-ast-height height)
  (let ([i 0]
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

(test (get-nil-ast-height (get-height (add (num 1) (num 2))))
      '(nil nil nil))

(test (get-nil-ast-height (get-height (add (num 1) (sub (num 3) (num 4)))))
      '(nil nil nil nil nil nil nil))

(test (get-nil-ast-height (get-height (add (add (num 1) (mult (num 5) (add (num 4) (num 2)))) (sub (num 3) (num 4)))))
      '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

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

;set-list-values :: ast list int-> list

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

;set-value-list :: list int list int -> list
;return a new list with value set in position definated by first int in the first list, given element in the second list by second int index
(define (set-value-list init-list init-index original-list original-index)
  (let ([selected-value (list-ref original-list original-index)]
        [new-list '()]
        [left-list '()]
        [right-list '()])
    (if (member selected-value TERMINAL_SET)
      (list-set init-list init-index selected-value)
      (begin
        (set! new-list (list-set init-list init-index selected-value))
        (set! left-list (set-value-list new-list (get-left-child-index init-index) original-list (get-left-child-index original-index)))
        (set! right-list (set-value-list left-list (get-right-child-index init-index) original-list (get-right-child-index original-index)))
        right-list
        ))
  ))

(test (set-value-list '(nil) 0 '(+ 1 2) 2)
      '(2))

(test (set-value-list '(+ 1 nil) 2 '(+ 1 5) 2)
      '(+ 1 5))

(test (set-value-list '(nil nil nil) 0 '(+ 1 5) 0)
      '(+ 1 5))


;get-sub-tree :: list int -> list
;return a sub list given a parent (list) and int like root
(define (get-sub-tree original-list root-index)
  (letrec ([subtree-height (height-sub-tree-list original-list root-index 0)]
           [subtree (get-nil-ast-height subtree-height)]
           [new-root (list-ref original-list root-index)])
    (set-value-list subtree 0 original-list root-index)
    )
  )

(test (get-sub-tree '(1) 0)
      '(1))

(test (get-sub-tree '(/ 2 3) 1)
      '(2))

(test (get-sub-tree '(+ 1 - nil nil 2 11) 2)
      '(- 2 11))

(test (get-sub-tree '(* 11 + nil nil - 7 nil nil nil nil 7 1 nil nil) 2)
      '(+ - 7 7 1 nil nil))

(test (get-sub-tree '(* 11 + nil nil 7 - nil nil nil nil nil nil 7 1) 2)
      '(+ 7 - nil nil 7 1))

;clean-subtree :: list int -> list
;return input list but subtree with nil values from index param
(define (clean-subtree init-list subtree-root)
  (let ([selected-value (list-ref init-list subtree-root)]
        [new-list (list-set init-list subtree-root 'nil)]
        [left-list '()])
    (if (member selected-value TERMINAL_SET)
        new-list
        (begin
          (set! left-list (clean-subtree new-list (get-left-child-index subtree-root)))
          (clean-subtree left-list (get-right-child-index subtree-root)))
        )
    ))

(test (clean-subtree '(1) 0)
      '(nil))

(test (clean-subtree '(+ 1 2) 1)
      '(+ nil 2))

(test (clean-subtree '(+ * / 1 2 5 7) 2)
      '(+ * nil 1 2 nil nil))


;list-to-ast :: list -> ast
;return ast list's version
#|
(define (list-to-ast ast-list)
  (void))


(test (list-to-ast '(1))
      (num 1))

(test (list-to-ast '(+ 1 2))
      (add (num 1) (num 2)))

(test (list-to-ast '(+ * 2 1 2 nil nil))
      (add (mult (num 1) (num 2)) (num 2)))
|#

;ast-to-list :: ast -> list
;return list ast's version
(define (ast-to-list ast)
  (set-ast-values ast (get-nil-ast-height (get-height ast)) 0))
(test (ast-to-list (add (num 4) (num 9)))
      '(+ 4 9))

(test (ast-to-list (num 4))
      '(4))

(test (ast-to-list (mult (num 2) (sub (num 9) (num 1))))
      '(* 2 - nil nil 9 1))