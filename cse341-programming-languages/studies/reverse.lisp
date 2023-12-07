;;; returns the reverse of the given list
(defun reverse-list (lst)
    (cond 
        ((eq lst nil) 
            nil)
        ((not (listp lst))
            lst)
        (t 
            (cons (reverse-list (cdr lst)) (reverse-list (car lst))))
    )
)

(setq my-list '(1 (A B) 3))
(print my-list)
(print (reverse-list my-list))