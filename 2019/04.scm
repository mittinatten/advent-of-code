(import test)
(import srfi-13)

(define (acceptable-password-4-1? password)
  (define (check-password password has-repeat prev-number)
    (if (string-null? password)
        (if has-repeat #t #f)
        (let ((number (string->number (string-take password 1))))
          (if (< number prev-number)
              #f
              (let ((next-has-repeat (if (or has-repeat
                                             (= prev-number number))
                                         #t #f)))
                (check-password (string-drop password 1) next-has-repeat number))
              ))))

  (check-password (string-drop password 1) #f (string->number (string-take password 1)))
  
  )
(define (filter-range first last pred)
  (if (> first last)
      '()
      (if (pred (number->string first))
          (cons first (filter-range (+ first 1) last pred))
          (filter-range (+ first 1) last pred))))

(test #t (acceptable-password-4-1? "111111"))
(test #f (acceptable-password-4-1? "223450"))
(test #f (acceptable-password-4-1? "123789"))

(test '(99 111) (filter-range 99 111 acceptable-password-4-1?))

;(print "4-1: " (length (filter-range 402328 864247 acceptable-password-4-1?)))

(define (acceptable-password-4-2? password)
  (define (check-password password has-pair streak prev-number)
    (if (string-null? password)
        (if (or has-pair (= streak 1)) #t #f)
        (let ((number (string->number (string-take password 1))))
          (if (< number prev-number)
              #f
              (let* ((same-as-prev (if (= prev-number number) #t #f))
                     (new-streak (if same-as-prev (+ streak 1) 0))
                     (new-has-pair (if (or has-pair (and (= streak 1) (not same-as-prev))) #t #f)))
                (check-password (string-drop password 1) new-has-pair new-streak number))
              ))))

  (check-password (string-drop password 1) #f 0 (string->number (string-take password 1))))

(test #t (acceptable-password-4-2? "112233"))
(test #f (acceptable-password-4-2? "123444"))
(test #t (acceptable-password-4-2? "111122"))

(print "4-2: " (length (filter-range 402328 864247 acceptable-password-4-2?)))
