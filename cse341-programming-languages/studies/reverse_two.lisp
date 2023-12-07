;; (reverse2 '(1 2 3 4 5 6 7)) => (2 1 4 3 6 5 7)
(defun reverse-two (lst)
    (cond 
        ((eq nil lst) nil)
        ((eq nil (cdr lst)) lst)
        (t  
            (cons (cadr lst) (cons (car lst) (reverse2 (cddr lst)))))))

(print (reverse-two '(1 2 3 4 5 6 7)))