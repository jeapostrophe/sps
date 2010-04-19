#lang scheme/base
; XXX De-optimize and use same posns as bulletml
; XXX Unitize over body
(require scheme/function
         scheme/list
         scheme/local
         scheme/contract
         scheme/unsafe/ops)

(define (unsafe-flsqr a)
  (unsafe-fl* a a))

(define (posn-distance p1 p2)
  (sqrt (unsafe-fl+ (unsafe-flsqr (unsafe-fl- (unsafe-vector-ref p1 0) (unsafe-vector-ref p2 0)))
                    (unsafe-flsqr (unsafe-fl- (unsafe-vector-ref p1 1) (unsafe-vector-ref p2 1))))))

(define-struct body (layer posn radius vel)
  #:prefab)

(define (body-posn-adjust! b dp)
  (define v1 (unsafe-struct-ref b 1))
  (define v2 dp)
  (unsafe-vector-set! v1 0 (unsafe-fl+ (unsafe-vector-ref v1 0) (unsafe-vector-ref v2 0)))
  (unsafe-vector-set! v1 1 (unsafe-fl+ (unsafe-vector-ref v1 1) (unsafe-vector-ref v2 1))))

(define (body-step! dx b)
  (define v1 (unsafe-struct-ref b 1))
  (define v2 (unsafe-struct-ref b 3))
  (unsafe-vector-set! v1 0 (unsafe-fl+ (unsafe-vector-ref v1 0) (unsafe-fl* dx (unsafe-vector-ref v2 0))))
  (unsafe-vector-set! v1 1 (unsafe-fl+ (unsafe-vector-ref v1 1) (unsafe-fl* dx (unsafe-vector-ref v2 1)))))

(define how-many-collision-tests 0)
(define (bodies-overlap? b1 b2)
  (define p1 (unsafe-struct-ref b1 1))
  (define p2 (unsafe-struct-ref b2 1))
  (define d (posn-distance p1 p2))
  (set! how-many-collision-tests (add1 how-many-collision-tests))  
  (unsafe-fl< d (unsafe-fl+ (unsafe-struct-ref b1 2) (unsafe-struct-ref b2 2))))

(define-struct simulation ([body-count #:mutable] layer-hash))
(define (create-simulation)
  (make-simulation 0 (make-hasheq)))
(define (simulation-add-body! sim b)
  (define layer-hash (simulation-layer-hash sim))
  (set-simulation-body-count! sim (add1 (simulation-body-count sim)))
  (hash-update! layer-hash (body-layer b)
                (curry list* b)
                empty))

(define-syntax-rule (body-for-each-corner! the-hash b y-ht expr ...)
  (local [(define the-b b)
          (define pos (unsafe-struct-ref the-b 1))
          (define rad (unsafe-struct-ref the-b 2))
          ; Compute corners
          (define min-x (unsafe-fl- (unsafe-vector-ref pos 0) rad))
          (define max-x (unsafe-fl+ (unsafe-vector-ref pos 0) rad))
          (define min-y (unsafe-fl- (unsafe-vector-ref pos 1) rad))
          (define max-y (unsafe-fl+ (unsafe-vector-ref pos 1) rad))
          ; Find cells
          (define min-x-c (comp->cell min-x))
          (define max-x-c (comp->cell max-x))
          (define min-y-c (comp->cell min-y))
          (define max-y-c (comp->cell max-y))
          ; Find hash components
          (define minx-ht (hash-ref! the-hash min-x-c make-hasheq))
          (define maxx-ht (hash-ref! the-hash max-x-c make-hasheq))
          ; Find second level hashs
          (define minx-miny-ht (hash-ref! minx-ht min-y-c make-hasheq))
          (define maxx-miny-ht (hash-ref! maxx-ht min-y-c make-hasheq))
          (define minx-maxy-ht (hash-ref! minx-ht max-y-c make-hasheq))
          (define maxx-maxy-ht (hash-ref! maxx-ht max-y-c make-hasheq))]
    ; Run expr ... on each corner
    (local [(define y-ht minx-miny-ht)]
      expr ...)
    (local [(define y-ht minx-maxy-ht)]
      expr ...)
    (local [(define y-ht maxx-miny-ht)]
      expr ...)
    (local [(define y-ht maxx-maxy-ht)]
      expr ...)))

(define cell-size 2.0)
(define (comp->cell x)
  (inexact->exact (floor (unsafe-fl/ x cell-size))))
(define (spatial-hash-collisions collide! sim)
  (define the-hash (make-hasheq))
  (define (add-body-to-hash! b)
    (define b1-layer (body-layer b))
    (body-for-each-corner!
     the-hash b y-ht
     (define old (hash-ref! y-ht b1-layer empty))
     (hash-set! y-ht b1-layer (list* b old))))
  (define seen?-ht (make-hasheq))
  (simulation-for-each-body! sim add-body-to-hash!)
  
  (for ([(b1-layer fbodies) (in-hash (simulation-layer-hash sim))])
    (hash-set! seen?-ht b1-layer #t)
    (for ([b1 (in-list fbodies)])
      (define b1-seen?-ht (make-hasheq))
      (body-for-each-corner!
       the-hash b1 y-ht
       (for* ([(b2-layer lbodies) (in-hash y-ht)]
              #:when (not (hash-has-key? seen?-ht b2-layer))
              [b2 (in-list lbodies)]
              #:when (not (hash-has-key? b1-seen?-ht b2)))
         (hash-set! b1-seen?-ht b2 #t)
         (when (bodies-overlap? b1 b2)
           (collide! b1 b2)))))))

(define (simulation-for-each-body! sim f)
  (for* ([bodies (in-hash-values (simulation-layer-hash sim))]
         [b (in-list bodies)])
    (f b)))

(define (simulation-bodies sim)
  (for*/list ([bodies (in-hash-values (simulation-layer-hash sim))]
              [b (in-list bodies)])
    b))

(define (simulate! collide! sim dx)
  (simulation-for-each-body! sim (curry body-step! dx))
  (set! how-many-collision-tests 0)
  (spatial-hash-collisions collide! sim)
  (printf "~a collisons tests in ~a bodies~n" how-many-collision-tests (simulation-body-count sim)))

(provide/contract
 [struct body ([layer symbol?]
               [posn (vector/c inexact-real? inexact-real?)]
               [radius inexact-real?]
               [vel (vector/c inexact-real? inexact-real?)])]
 [body-posn-adjust! (body? (vector/c inexact-real? inexact-real?) . -> . void)]
 [create-simulation (-> simulation?)]
 [simulation-add-body! (simulation? body? . -> . void)]
 [simulation-bodies (simulation? . -> . (listof body?))]
 [simulate! ((body? body? . -> . void)
             simulation?
             inexact-real?
             . -> .
             void)])