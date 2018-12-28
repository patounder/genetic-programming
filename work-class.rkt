#lang play
;IMPORTS
(require "member-dto.rkt")
(require "interp-service.rkt")
;CONSTANTS
(define DEPTH_MAX_AST 3)
(define OPERATIONS_IN_LANGUAGE '(+ -))
(define TERMINALS_IN_LANGUAGE '(0 1 2 3 4 5 6 7 8 9 10))
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
return the best-so-far individual (may be is necessary transform AST to concrete sintax).
|#

;get-random-operations-list :: int list-> list (from OPERATIONS_IN_LANGUAGE)
;return list (with size from DEPTH_MAX_AST param) with operations in that order
(define (get-random-operations-list depth_max_ast init_list)
  (if (zero? depth_max_ast)
      init_list
      (get-random-operations-list (sub1 depth_max_ast) (cons
                                                        (list-ref OPERATIONS_IN_LANGUAGE (random (length OPERATIONS_IN_LANGUAGE)))
                                                        init_list))
      )
  )

;create-random-member :: (void) -> class-member-dto instance
;return a new instance for class-member-dto with random ast
(define (create-random-member)

  ;create-random-ast :: int -> expr
  (define (create-random-ast max_depth)
    ;TODO iterate until depth is zero. Select quantity of operations for each depth grade
    ;TODO create nodes necessary for each selected operation
    (define (create-random-ast-depth depth ast)
      (void)
      )
    
    (add (num 1) (num 3)))

  (new-instance class-member 0 (create-random-ast DEPTH_MAX_AST))
  )

;create_population :: int -> list of object-class-member (with random ast)
;function that generate population (or member list) with random ast or program
(define (create-population qtty-members)

  (define (create-pop qtty init-pop)
    (if (equal? qtty 0)
        init-pop
        (create-pop (- qtty 1) (cons (create-random-member) init-pop))
        )
    )
  
  (create-pop qtty-members '()))

