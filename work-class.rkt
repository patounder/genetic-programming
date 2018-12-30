#lang play
;IMPORTS
(require "member-dto.rkt")
(require "interp-service.rkt")


;CONSTANTS
(define DEPTH_MAX_AST 3)
(define NON_TERMINAL_SET '(+ - *))
(define TERMINAL_SET '(1 2 5 7 9))
(define FULL_SET (append NON_TERMINAL_SET TERMINAL_SET))
(define QTTY_POPULATION 100)
(define MAX_ITERATIONS 100)
(define FIND_VALUE 19)
(define INIT_BEST_FITNESS 10000)

;MACROS
(defmac (my-while cond body)
  (letrec ([iter (λ()
                   (if cond
                       (begin
                         body
                         (iter))
                       (void)))])
    (iter)))

#|----------------------------------------------------------------------------------------------------------------------------------------------------------------
TODOS:

1 Randomly create an initial population of programs from the available primitives. Initial and futures. 
1.1 set data structure for support (attributes member, depth)
repeat
 2 Execute each program and ascertain its fitness. Not compile, compile but result not ok, result ok. Use try catch. Count errors by step lexic, semantic, and so on.
 3 Select one or two program(s) from the population with a probability based on fitness to participate in genetic operations (Section 2.3).
 4 Create new individual program(s) by applying genetic operations with specified probabilities (Section 2.4).
   4.1 Crossover
   4.2 Mutation
until an acceptable solution is found or some other stopping condition is met (e.g., a maximum number of generations is reached).
return the best-so-far individual (may be is necessary transform AST to concrete sintax).
----------------------------------------------------------------------------------------------------------------------------------------------------------------|#

;grow :: int -> ast
;return ast from set available (with terminal and non-terminal nodes). Implement grow algorithm
(define (grow depth)
  (if (= depth DEPTH_MAX_AST)
      (parse (list-ref TERMINAL_SET (random 0 (length TERMINAL_SET))))
      (let [(selected_node (list-ref FULL_SET (random 0 (length FULL_SET))))]
        (if (member selected_node TERMINAL_SET)
            (parse selected_node)
            (let [(l-branch (grow (add1 depth)))
                  (r-branch (grow (add1 depth)))];In this case all operations (+ and -) use two branch, but in others is necessary evaluate 
              (if (equal? selected_node '+)
                  (add l-branch r-branch)
                  (sub l-branch r-branch))
              )
            )
        )
      )
  )

;create-random-member :: (void) -> class-member-dto's object
;return a new instance for class-member-dto with random ast
(define (create-random-member reference-value)
  (letrec ([ast (grow 0)]
           [value (interp ast)]
           [fitness (abs (- value reference-value))])
    (new-instance class-member fitness ast value))
  )

;select-best-member :: list of member -> class-member-dto's object
;return member with best fitness in generation
(define (select-best-member member-list)
  (let ([best-member (new-instance class-member INIT_BEST_FITNESS '() '())])
    (begin
      (for-each (λ(member)
                  (when (< (send 'get-fitness member) (send 'get-fitness best-member))
                    (set! best-member member)
                    ))
                member-list)
      best-member)))

;create_population :: int -> class-generation's object 
;function that generate population (or member list) with random ast or program
(define (create-population qtty-members reference-value)

  ;create-pop :: int list -> list
  (define (create-pop qtty init-pop)
    (if (= qtty 0)
        init-pop
        (create-pop (- qtty 1) (cons (create-random-member reference-value) init-pop)))
    )
  
  (letrec ([member-list (create-pop qtty-members '())]
           [best-member (select-best-member member-list)])
    (new-instance class-generation member-list best-member)
    ))

(define (main)
  (letrec ([init-population (create-population QTTY_POPULATION FIND_VALUE)]
           [chosen-population init-population]
           [best-member (send 'get-best-member chosen-population)]
           [actual-list-member '()]
           [it 0])
    (send 'get-fitness best-member)
    (send 'get-ast best-member)
    #;(my-while (< it MAX_ITERATIONS)
              (begin
                ;2 Execute each program and ascertain its fitness. Not compile, compile but result not ok, result ok. Use try catch. Count errors by step lexic, semantic, and so on.
                (set! actual-list-member (foldr (λ (member l)
                                                  (cons (new-instance class-member 0 (send 'get-ast member) (interp (send 'get-ast member))) l))
                                                '() (send 'get-member-list init-population)))
                (set! it (add1 it))))
    )
  )