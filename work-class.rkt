#lang play
;IMPORTS
(require "member-dto.rkt")
(require "interp-service.rkt")


;CONSTANTS
(define DEPTH_MAX_AST 3)
(define NON_TERMINAL_SET '(+ -))
(define TERMINAL_SET '(0 1 2 3 4 5 6 7 8 9 10))
(define FULL_SET (append NON_TERMINAL_SET TERMINAL_SET))
(define QTTY_POPULATION 100)


#|----------------------------------------------------------------------------------------------------------------------------------------------------------------
TODOS:

1 Randomly create an initial population of programs from the available primitives. Initial and futures. 
1.1 set data structure for support (attributes member, depth)
repeat
 2 Execute each program and ascertain its fitness. Not compile, compile but result not ok, result ok. Use try catch. Count errors by step lexic, semantic, and so.
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
                  (r-branch (grow (add1 depth)))]
              (if (equal? selected_node '+)
                  (add l-branch r-branch)
                  (sub l-branch r-branch))
              )
            )
        )
      )
  )

;create-random-member :: (void) -> class-member-dto instance
;return a new instance for class-member-dto with random ast
(define (create-random-member)
  (new-instance class-member 0 (grow 1)))

;create_population :: int -> class-generation's object 
;function that generate population (or member list) with random ast or program
(define (create-population qtty-members)

  ;create-pop :: int list -> list
  (define (create-pop qtty init-pop)
    (if (= qtty 0)
        init-pop
        (create-pop (- qtty 1) (cons (create-random-member) init-pop))
        )
    )
  (let ([member-list (create-pop qtty-members '())])
    (new-instance class-generation member-list '())
    ))
