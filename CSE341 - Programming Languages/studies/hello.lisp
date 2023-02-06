(defun hello ()
    (format t "Hello, Whats your first name? ")
    (setq *name* (read))
    (format t "Nice to meet you, ~a ~%" *name*)
)

(hello)