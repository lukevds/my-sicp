;; -*- lexical-binding: t -*-
;; peanutbrain pilled take
;; variable names are already stored in a data structure homologous
;; to a hash-table
;; (defvar memo-fun-table (make-hash-table)
;;   "hash table for `memo-fun'. do not access directly!")

;; (defun memo-fun (fun)
;;   (lambda ()
;;     (let ((fun-value (gethash fun memo-fun-table)))
;;       (if fun-value
;; 	  (cdr fun-value)
;; 	(puthash fun (cons 'value (funcall fun)) memo-fun-table)))))

(defun memo-fun (fun)
  (let ((already-run)
	(result))
    (lambda ()
      (if already-run
	  result
	(setq already-run t
	      result (funcall fun))))))

(defmacro delay (exp)
  `(memo-fun (lambda () ,exp)))

(defun force (delayed-exp)
  (funcall delayed-exp))

(defvar the-empty-stream nil)

(defun empty-streamp (obj)
  (eq obj nil))

(defmacro stream-cons (scar scdr)
  `(cons ,scar
	 (delay ,scdr)))

(defun stream-car (stream)
  (if (and (consp stream) (functionp (cdr stream)))
      (car stream)
    (error "obj passed to `stream-car' is not a stream")))

(defun stream-cdr (stream)	
  (if (and (consp stream) (functionp (cdr stream)))
      (force (cdr stream))
    (error "obj passed to `stream-car' is not a stream")))

(defun stream-nthcdr (stream n)
  (let ((stream stream))
    (while (> n 0)
      (setq stream (stream-cdr stream)
	    n (1- n)))
    stream))

(defun stream-ref (stream n)
  (stream-car (stream-nthcdr stream n)))

;; (defun stream-ref (stream n)
;;   (let ((-scar (stream-car stream))
;; 	(-scdr (stream-cdr stream)))
;;     (while (> n 0)
;;       (setq -scar (stream-car -scdr)
;; 	    -scdr (stream-cdr -scdr)
;; 	    n (1- n)))
;;     -scar))

;; (defun stream-map (stream fun)
;;   (let ((fun (if (symbolp fun)
;; 		 (symbol-function fun)
;; 	       fun)))
;;     (stream-cons
;;      (funcall fun (stream-car stream))
;;      (stream-map (stream-cdr stream) fun))))

(defun stream-filter (filter-fun stream)
  (if (funcall filter-fun (stream-car stream))
      (stream-cons
       (stream-car stream)
       (stream-filter filter-fun (stream-cdr stream)))
    (stream-filter filter-fun (stream-cdr stream))))


;; 3.50
(defun stream-map (fun &rest streams)
  (if (null streams)
      the-empty-stream
    (stream-cons (apply fun (mapcar (symbol-function 'stream-car)
				    streams))
		 (apply (symbol-function 'stream-map)
			fun
			(mapcar (symbol-function 'stream-cdr)
				streams)))))


(defun stream-take (stream n)
  (let ((stream-list (list (stream-car stream)))
	(stream (stream-cdr stream)))
    (while (> n 1)
      (setq stream-list (cons (stream-car stream)
			      stream-list)
	    stream (stream-cdr stream)
	    n (1- n)))
    (nreverse stream-list)))

(defun stream-find (stream element &optional comparator)
  (let ((current-element (stream-car stream))
	(comparator      (or comparator (symbol-function 'eq))))
    (while (not (funcall comparator current-element element))
      (setq current-element (stream-car (stream-cdr stream))
	    stream (stream-cdr stream)))
    current-element))

(defun integers-starting-from-n (n)
  (stream-cons n (integers-starting-from-n (1+ n))))

(defun fibgen (a b)
  (stream-cons a (fibgen b (+ a b))))

;; why does this work?
(setq powers-2
      (stream-cons
       1
       (stream-map (lambda (n) (* n 2)) powers-2)))

(setq ones
      (stream-cons 1 ones))


(setq nats (integers-starting-from-n 0))

(setq evens (stream-map (lambda (num) (* num 2)) nats))
(setq odds (stream-filter (lambda (num) (> (% num 2) 0)) nats))

(stream-take (stream-map (lambda (m n) (list m n)) odds evens) 1000)
(stream-car (stream-cdr (stream-cdr (stream-cdr nats))))

(stream-take odds 5)

(setq negative-integers (stream-map (lambda (n) (- n)) (stream-cdr nats)))

(stream-take negative-integers 100)

(setq fibs (fibgen 0 1))

(setq integers (stream-cons 1 (stream-map (lambda (n) (1+ n)) integers)))

(stream-take integers 10)
;; does stream memory usage increase with size? how?

;; 3.53
'(define s (cons-stream 1 (add-streams s s)))
powers of 2
(stream-take (let ((s)) (setq s (stream-cons 1 (stream-map (symbol-function '+) s s)))) 10)

;; 3.54
(defun mul-streams (s1 s2)
  (stream-map (symbol-function '*) s1 s2))

(setq factorials
      (stream-cons
       1
       (mul-streams factorials
		    (integers-starting-from-n
		     (stream-car factorials)))))

(stream-take factorials 10)

;; 3.55
(defun partial-sums (stream)
  (let ((new-stream))
    (setq new-stream
	  (stream-cons
	   (stream-car stream)
	   (stream-map (symbol-function '+) new-stream (stream-cdr stream))))))

(stream-take (partial-sums (stream-cdr nats)) 20)
; (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)

;; 3.56
(defun stream-scale (stream n)
  (let ((mul-subr (symbol-function '*)))
    (stream-map mul-subr
		stream
		(stream-map (lambda (x) (* x n)) ones))))
; (stream-take (stream-scale nats 10) 10)

(defun stream-merge (s1 s2)
  (cond ((empty-streamp s1) s2)
	((empty-streamp s2) s1)
	(:else
	 (let ((cars1 (stream-car s1))
	       (cars2 (stream-car s2)))
	   (cond ((< cars1 cars2)
		  (stream-cons cars1 (stream-merge (stream-cdr s1) s2)))
		 ((> cars1 cars2)
		  (stream-cons cars2 (stream-merge s1 (stream-cdr s2))))
		 (:else
		  (stream-cons cars1
			       (stream-merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(setq positive-numbers (stream-cdr nats))
(setq S
      (stream-cons
       1
       (stream-merge
	(stream-scale S 2)
	(stream-merge (stream-scale S 3) (stream-scale S 5)))))

(stream-ref S 100)
(stream-take
 (stream-map (lambda (a b) (list a b))
	     positive-numbers
	     S)
 25)

(setq multiples-of-3
      (stream-cons
       3
       (stream-map (lambda (n) (+ n 3)) multiples-of-3)))

(stream-take multiples-of-3 6)

;; 3.58
        num den radix
(expand 2   3   4)
(/ (* 2 4) 3) = (/ 8 3) = 2
(expand (% (* 2 4) 3) 3 4) = (expand 2 3 4)


        num den radix
(expand 1   7   10)
(/ (* 1 10) 7) = (/ 10 7) = 1;
(expand (% (* 1 10) 7) 7 10) = (expand (% 10 7) 7 10) = (expand 3 7 10)

(expand 3   7   10) = (/ (* 3 10) 7) = 4;
(expand (% (* 3 10) 7) 7 10) = (expand 2 7 10)

(expand 2 7 10) = (/ (* 2 10) 7) = 2;
(expand (% (* 2 10) 7) 7 10) = (expand 6 7 10)

(expand 6 7 10) = (/ (* 6 10) 7) = 8;
(expand (% (* 6 10) 7) 7 10) = (expand 4 7 10)

(expand 4 7 10) = (/ (* 4 10) 7) = 5;
(expand (% (* 4 10) 7) 7 10) = (expand 5 7 10)

(expand 5 7 10) = (/ (* 4 10) 7) = 5;
(expand (% (* 5 10) 7) 7 10) = (expand 1 7 10)

(defun expand (num den radix)
  (stream-cons
   (/ (* num radix) den)
   (expand (% (* num radix) den) den radix)))

(setq nsei (expand 1 7 10))
(stream-take nsei 20) ;; => (1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4)

(setq outro (expand 3 8 10))
(stream-take outro 20) ;; => (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;; this function returns a stream of digits after period of the
;; division `num'/`den' in base `radix'

(setq ayy (expand 3 8 2))
(stream-take ayy 20) => (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

(defun double-transform (stream)
  (stream-cons (* (stream-car stream) 2)
	       (double-transform 
		(stream-map (lambda (n) (* n 2)) (stream-cdr stream)))))

(defun make-tableau (transform stream)
  (stream-cons stream
	       (make-tableau transform
			     (funcall transform stream))))

(defun accelerated-sequence (transform stream)
  (stream-map (symbol-function 'stream-car)
              (make-tableau transform stream)))

(stream-take (double-transform integers) 20)
(2 8 24 64 160 384 896 2048 4608 10240 22528 49152 106496 229376 491520 1048576 2228224 4718592 9961472 20971520)
(stream-take
 (accelerated-sequence
  (symbol-function 'double-transform)
  (double-transform integers))
 10)
