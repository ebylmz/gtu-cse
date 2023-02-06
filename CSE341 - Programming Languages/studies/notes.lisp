(defmacro addition (a b)
    "adds two number"
    (+ a b)
)

(write (addition 5 6))

;; to prevent expression evaluation use single quote(') 
(write '(addition 5 6))

(setq a 20)
(cond 
    ((> a 20) (format t "~%a is greater than 20"))
    ((< a 20) (format t "~%a is smaller than 20"))
    (t (format t "~%a is equal to 20"))
)

(when (< a 20)
    (format t "~%a is equal to 20")
)

(defun averagenum (n1 n2 n3 n4)
    (/ (+ n1 n2 n3 n4) 4)
)

(print (averagenum 1 2 3 4))


;; anonymous functions
(write
    ((lambda (x y z) (+ (* x x) (* y y) (* z z))) 3 2 1)
)


(defun cubeMyList (lst)
    (mapcar #' (lambda (x) (* x x x)) lst)
)

(write (cubeMyList '(1 2 3 4 5)))

(write (mapcar '1+ '(1 3 5 7 9)))

(write (mapcar '+ '(1 3 5 7 9) '(2 4 6)))

; '(1 2 3 4) returns list (1 2 3 4)
;  (1 2 3 4) tries calling function named 1 with parameters 2, 3, and 4


;; keyword parameters
(defun show-members (&key a b c d) 
    (write (list a b c d))
)

(show-members :a 1 :c 2 :d 3)


;; parallel binding for x and y
(let (var (x 4) (y 5))
    (princ x)
    (terpri) ; (princ #\newline)
    (princ y)
    (terpri)
)

;; sequential binding for x and y
(let* (var (x 4) (y x))
)

;; equvalent to x = 5;
(let (x) 
    (setq x 5))