(defun palindrome (str)
    (setf len (length str))
    (cond 
        ((or (= len 0) (= len 1))
            t)
        ((eq (char str 0) (char str (- len 1)))
            (palindrome (subseq str 1 (- len 1))))
        (t
            nil)
    )
)

(print (palindrome "atata"))