(defun cube-list (lst)
    (mapcar #'(lambda (x) (* x x x)) lst)
)

(defun combine (lst1 lst2)
    (mapcar '+ lst1 lst2)
)

;;; A function that filters a given list returning all the elements bigger than 10 and smaller than 50
(defun filter (lst fil)
    (mapcar #'(lambda (x) (if (< x 50) x)) lst)
)

(setf mylist '(1 2 3 4 5))
(print (cube-list mylist))
(print (combine mylist '(1 2 3 4 43)))
(print (filter mylist '(1 2 3 4 43)))