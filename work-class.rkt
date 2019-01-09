#lang play
;IMPORTS
(require "member-dto.rkt")
(require "interp-service.rkt")
(require "tree-population-manager.rkt")


;CONSTANTS
(define DEPTH_MAX_AST 3)
(define FULL_SET (append NON_TERMINAL_SET TERMINAL_SET))
(define QTTY_POPULATION 3)
(define MAX_ITERATIONS 100)
(define FIND_VALUE 19)
(define QUANTITY_PARENTS 50)
(define TOURNAMENT_NUMBER_ATTEMPTS 70)
(define INIT_BEST_FITNESS 10000)

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

;main :: void -> void
;execute programming genetic
(define (main);TODO implement part II (analize again)
  (letrec ([population (create-population QTTY_POPULATION FIND_VALUE)]
           [chosen-population population]
           [parent-list '()]
           [it 0])
    (my-while (and (> (send 'get-fitness (send 'get-best-member chosen-population)) 0) 
                   (< it MAX_ITERATIONS)) 
              (begin
                (set! parent-list (select-parents population))
                ;TODO reproduction, build new population
                ;TODO Define fitness with result or value and/or number of errors. Compare bestmember between chosen-population and new population.
                ;TODO iterate until reach limits (achieve desired value or max iterations)
                ;TODO present best member (in chosen population). Print time, ast, final result (concrete sintax)
                (void))
              )))

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

;create-random-member :: (void) -> class-member-dto instance
;return a new instance for class-member-dto with random ast
(define (create-random-member reference-value)
  (letrec ([ast (grow 0)]
           ;[ast-list ()]
           [value (interp ast)]
           [fitness (abs (- value reference-value))])
    (new-instance class-member fitness ast value))
  )

;select-best-member :: list of member -> class-member-dto instance
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

;create_population :: int -> class-generation instance 
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

;select-parents :: class-generation instance -> list (member instance)
;return selected parents from generation with best fitness
(define (select-parents generation)
  (let ([it 0]
        [member-list (send 'get-member-list generation)]
        [parents-list '()])
    (begin
      (my-while (< it QUANTITY_PARENTS)
                (cons (tournament-selection member-list) parents-list))
      parents-list)
    ))

;tournament-selection :: list (member instance) -> member instance
;return member with best fitness in selected
(define (tournament-selection member-list)
  (let ([it 0]
        [best-member (car member-list)]
        [mem-random-select (void)])
    (begin
      (my-while (< it TOURNAMENT_NUMBER_ATTEMPTS)
                (begin
                  (set! mem-random-select (list-ref member-list (random (length member-list))))
                  (when (< (send 'get-fitness mem-random-select) (send 'get-fitness best-member))
                    (set! best-member mem-random-select))))
      best-member)
    ))

;reproduction :: list (of member instance) int -> class-generation instance
;return new generation with members list created from parent-list
(define (reproduction parents)
  (let ([it 0]
        [best-member (new-instance class-member INIT_BEST_FITNESS empty empty)]
        [child empty]
        [child-list empty]
        [mother-random empty]
        [father-random empty])
    (begin
      (my-while (< it QTTY_POPULATION)
                (begin
                  (set! mother-random (list-ref parents (random (length parents))))
                  (set! father-random (list-ref parents (random (length parents))))
                  (set! child (make-child mother-random father-random))
                  (cons child child-list)
                  (when (< (send 'get-fitness child)
                           (send 'get-fitness best-member))
                    (set! best-member child))
                  ))
      (new-instance class-generation child-list best-member))))

;make-child :: member member -> member
;return new member instance, apply crossover (child) between mother and father instances, then mutation in new child
(define (make-child mother father)
  (let (#;[mother-like-list (get-nil-ast-height (get-height mother))])
    (begin
      (def new-child (crossover mother father))
      (mutation new-child)
      )))


;crossover :: member member -> member
;return a new member instance with copy and mix genes from params (mother and father)
(define (crossover mother father)      
  (letrec ([child-list (set-ast-values (send 'get-ast mother) (get-nil-ast-height (get-height (send 'get-ast mother))) 0)]
           [crossover-point (random (length child-list))]
           ;TODO create father's subtree defined with root from crossover-point in the offspring, must be contain a height defined with height from crossover point in the child's subtree
           ;TODO remove because list must already exist in the object
           [father-like-list (set-ast-values (send 'get-ast father) (get-nil-ast-height (get-height (send 'get-ast father))) 0)]
           [father-random-subtree (get-sub-tree father-like-list (random (length father-like-list)))]
           [clean-child-list (clean-subtree child-list crossover-point)]
           )
    (set-value-list clean-child-list crossover-point father-random-subtree 2)))

(define mother-member (new-instance class-member 0 '(+ 1 2) (add (num 1) (num 2)) 3))
(define father-member (new-instance class-member 0 '(* + - 1 5 9 7) (mult (add (num 1) (num 5)) (sub (num 9) (num 7))) 10))
(crossover mother-member father-member)
      

;mutation :: member -> member
;return a new instance member with crossover random subtree
(define (mutation init-mem)
  ;TODO mutation (use probability param)
         ;TODO select point in the child, and the subtree
         ;TODO generate random-ast
         ;TODO represent random-ast like list
      ;TODO clean subtree with crossover point like root. Like in previous crossover point
      ;TODO set values in the child or replace tree with random generated. (crossover idea but with father random tree and the maximium height
      ;TODO generate random-ast
      ;TODO represent random-ast like list
  (new-instance class-member '() '() '() '())
  )