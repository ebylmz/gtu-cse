; car returns the first element of the list
; cdr returns the list without the first element
; cons returns the list by inserting given item at the first place 

(defun isMember (lst item)
    (cond 
        ((eq lst nil) nil)
        ((eq (car lst) item) t)
        (t (isMember (cdr lst) item))
    )
)

(defun isEqual (lst1 lst2)
    (cond
        ((eq lst1 nil) (eq lst2 nil))
        ((eq lst2 nil) nil)
        ((eq (car lst1) (car lst2)) (isEqual (cdr lst1) (cdr lst2)))
        (t nil)
    )
)

(defvar lst1 '(1 2 3))
(defvar lst2 '(1 2 3))

;; (write (isMember lst1 5))
;; (write (cons '(a b) '(c d)))