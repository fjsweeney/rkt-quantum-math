#lang racket

(require racket/vector)
(require math)
(require racket/function)

;; practice section
(define add
 (lambda vs
  (apply vector-map + vs)))

(define (negate v)
 (vector-map - v)))

(define (scale c v)
 (vector-map (lambda (n) (* c n)) v))

;; A Tensor is a vector that:
;; A) contains the same type as all other vectors at the same nesting depth.
;; B) are the same size as all other vectors at the same nesting depth.

(define tensor-add
 (lambda ts
  (cond
   ([vector? (car t1)] (apply vector-map tensor-add ts))
   (#t (apply vector-map + ts)))))

(define (tensor-map f t)
 (cond
  ([vector? (car t)] 
   (vector-map (curry tensor-map f) t))
  (#t (vector-map f t))))

(define (tensor-scale c t)
 (tensor-map (curry * c) t)

(define (tensor-negate t)
 (tensor-scale -1 t))

(define tensor-conjugate 
 (curry tensor-map conjugate))

(define (vector-apply f v)
 (apply f (vector->list v)))



;; main section


(define adjoint matrix-hermitian)

(define hadamard-matrix (matrix-scale (/ 1 (sqrt 2)) #[#[1 1]#[1 -1]]))

(define (standard-inner-product v1 v2)
 (matrix* v1 (matrix-transpose v2)))

(define (hermetian? m)
 (matrix= (matrix-hermitian m) m))

(define (symmetric? m)
 (matrix= (matrix-transpose m) m))

(define (unitary? m)
 (matrix-identity? (matrix* m (matrix-hermitian m))))

(define (mod-squared c)
 (magnitude (* c c)))

(define (doubly-stochastic? m)
 (and 
  (equal? 1 (array-axis-sum m 1))
  (equal? 1 (array-axis-sum m 2))))

(define (kronecker-blocks m1 m2)
 (matrix-map 
  (lambda (a) (matrix-scale m2 a)) 
  m1))

(define (kronecker-product m1 m2)
 (list*->matrix 
  (matrix->list 
   (matrix-map matrix->list 
               (kronecker-blocks m1 m2)))))


(define test-matrix1 
 (matrix-transpose 
  (matrix 
[[0  (/ 1 2) (/ 1 2) 0 0 0 0 0]
 [0 0 0 (/ 1 3) (/ 1 3) (/ 1 3) 0 0]
 [0 0 0 0 0 (/ 1 3) (/ 1 3) (/ 1 3)]
 [0 0 0 1 0 0 0 0]
 [0 0 0 0 1 0 0 0]
 [0 0 0 0 0 1 0 0]
 [0 0 0 0 0 0 1 0]
 [0 0 0 0 0 0 0 1]])))

(define (probabilities state)
 (let ([n (expt (matrix-2norm state) 2)])
  (matrix-map (lambda (a) (/ (mod-squared a) n)) state)))

;;*** needs eigenvalues? ***
;;(define (expected-val obs state)
;; (matrix* (matrix-hermitian (matrix* obs state)) state))



(define test-obs (matrix [[1 0-i][0+i 2]]))
(define test-ket (col-matrix [(/ (sqrt 2) 2) (/ (sqrt 2) 0+2i)]))
