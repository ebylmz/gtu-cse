(defconstant *sym-type-var* "v")
(defconstant *sym-type-fun* "f")

(defun sym-list-id (sym-list) 
    (nth 0 sym-list))

(defun sym-list-type (sym-list) 
    (nth 1 sym-list))

(defun sym-list-val (sym-list) 
    (nth 2 sym-list))

(defun get-sym (sym-tab id &optional (sym-type *sym-type-var*)) ;; is this function or variable
    (cond 
        ((null sym-tab) 
            nil)
        ((and (string= id (caar sym-tab)) (string= sym-type (sym-list-type (car sym-tab)))) 
            (nth 2 (car sym-tab)))
        (t 
            (get-sym (cdr sym-tab) id))))

(defun def-sym (sym-tab id sym-type val)
    (cond 
        ((null sym-tab) 
            (list (list id sym-type val)))
        ((and (string= id (sym-list-id (car sym-tab))) (string= sym-type (sym-list-type (car sym-tab)))) 
            sym-tab)
        (t 
            (setf (cdr sym-tab) (def-sym (cdr sym-tab) id sym-type val))
            sym-tab)))

(defun set-sym (sym-tab id val)
    (cond 
        ((null sym-tab) 
            nil)
        ((and (string= id (sym-list-id (car sym-tab))) (string= *sym-type-var* (sym-list-type (car sym-tab)))) 
            (setf (nth 2 (car sym-tab)) val) val)
        (t 
            (set-sym (cdr sym-tab) id val))))

(defun print-sym-tab (sym-tab)
	(format t "~%~30s~15s~t~s~%" "SYMBOL" "TYPE" "VALUE");
    (print-sym-tab-helper sym-tab))

(defun print-sym-tab-helper (sym-tab) 
    (cond 
        ((null sym-tab) nil)
        (t 
            (let ((sym-list (car sym-tab)))
                (format t "~30s~15s~t~s~%" 
                    (sym-list-id sym-list) 
                    (if (string= (sym-list-type sym-list) *sym-type-var*) "VARIABLE" "FUNCTION") 
                    (sym-list-val sym-list)))
            (print-sym-tab-helper (cdr sym-tab)))))