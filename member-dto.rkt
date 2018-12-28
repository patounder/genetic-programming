#lang play

(define (new-instance class . parameters)
  (apply class parameters))

(define (send message object . args)
  (let ((method (object message)))
    (cond ((procedure? method) (apply method args))
          (else (error "Error in method lookup " method)))))

(define (class-member fitness ast)
 (let (
       (fitness fitness)
       (ast ast)
       )     
   (define (get-fitness) fitness)
   (define (get-ast) ast)
   (define (self message)
     (cond ((eqv? message 'get-fitness) get-fitness)
           ((eqv? message 'get-ast) get-ast)
	   (else (error "Undefined message" message))))
   self))

(define first-member (new-instance class-member 10 '(1 2 3)))

;(send 'get-fitness first-member)

(define (class-generation member-list best-member)
 (let ((member-list member-list)
       (best-member best-member))     
   (define (get-member-list) member-list)
   (define (get-best-member) best-member)
   (define (self message)
     (cond ((eqv? message 'get-member-list) get-member-list)
           ((eqv? message 'get-best-member) get-best-member)
	   (else (error "Undefined message" message))))
   self))