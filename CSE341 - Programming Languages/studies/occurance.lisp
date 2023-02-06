(defun occurance (lst target)
    (cond 
        ((eq lst nil) 
            0)   
        ((listp (car lst))
            (+ (occurance (car lst) target) (occurance (cdr lst) target)))
        (t
            (+ (if (eq target (car lst)) 1 0) (occurance (cdr lst) target)))
    )
)


(setf mylist '(1 2 3 4 2 5 (2 2)))
(print (occurance mylist 2))