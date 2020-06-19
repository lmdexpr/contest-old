(define (solve s)
  (let ((a (make-vector (vector-length s) 1))
        (n (vector-length s)))
    (dotimes (i (- (vector-length s) 2))
      (if (and (vector-ref s i) (vector-ref s (+ i 1)))
        (begin
          (set! (vector-ref a (+ i 2)) (+ (vector-ref a i) (vector-ref a (+ i 2))))
          (set! (vector-ref a i) 0)))
      (if (not (or (vector-ref s (- (- n i) 1)) (vector-ref s (- (- n i) 2))))
        (begin
          (set! (vector-ref a (- (- n i) 3)) (+ (vector-ref a (- (- n i) 1)) (vector-ref a (- (- n i) 3))))
          (set! (vector-ref a (- (- n i) 1)) 0))))
    (map number->string (vector->list a))))

(print (string-join (solve (vector-map (lambda (x) (char=? x #\R)) (string->vector (read-line)))) " "))
