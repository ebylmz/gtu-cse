(defconstant *output-file-name* "output.txt")
(defvar *sym-tab* nil)
(defvar *exit-prog* nil)
(defvar *error-flag* nil)
(defvar *error-type* nil)

(load "gpp_lexer.lisp")
(load "sym_tab.lisp")

(defun gppinterpreter (&optional file-names)
    (let ((out-stream (open *output-file-name* :direction :output)))
        (if file-names 
            (mapcar #'(lambda (file-name) (gpp-parser (gpp-lexer file-name) out-stream)) file-names)
            (gpp-parser (gpp-lexer) out-stream))
        (close out-stream)))

(defun gpp-parser (tokens out-stream)
    (if (null tokens) (return-from gpp-parser nil))
    (let ((result nil) (out-val nil))
        (setq result ($START tokens))
        (setq out-val (car result))
        (if out-val
            (progn
                (format out-stream "Syntax OK.~%Result: ~s~%" 
                    (if (numberp out-val) (format-valuef out-val) out-val))
                (if (not *exit-prog*)
                    (gpp-parser (cadr result) out-stream)))
            (format out-stream "SYNTAX_ERROR Expression not recognized.~%Result: ~s~%" result))
        (car result)))

(defun $START (tokens)
    ($INPUT tokens))

(defun $INPUT (tokens)
    (let ((result))
        (cond 
            ((car (setq result ($EXIT tokens))) result)
            ((car (setq result ($EXP tokens))) result)
            ((car (setq result ($EXPB tokens))) result) ;; new rule added !
            ((car (setq result ($EXPLIST tokens))) result)
            ((car (setq result ($FUNCTION tokens))) result))))

(defun $EXIT (tokens) 
    ;; (format t "EXP1.tokens: ~s~%" tokens) 
    (let ((result nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXIT (interp-result nil tokens))) 
        ;; 1
        (if (string= "KW_EXIT" (next-token tokens)) 
            (progn
                (setq op (next-token tokens))
                (setq tokens (cdr tokens)))
            (return-from $EXIT (interp-result nil tokens)))
        ;; 2
        (if (string= (next-token tokens) "OP_CP") 
            (progn
                (setq *exit-prog* t)
                (setq result (interp-result "1f0" tokens)))
            (return-from $EXIT (interp-result nil tokens)))
        result))


(defun $EXP (tokens) 
    (let ((result))
        (cond 
            ((car (setq result ($ID tokens))) result)
            ((car (setq result ($VALUEF tokens))) result)
            ((car (setq result ($EXP1 tokens))) result)
            ((car (setq result ($EXP2 tokens))) result)
            ((car (setq result ($EXP3 tokens))) result)
            ((car (setq result ($EXP4 tokens))) result)
            ((car (setq result ($FCALL tokens))) result)
            (t (interp-result nil tokens)))))

;; EXP := OP_OP (OP_PLUS | OP_MINUS | OP_MULT | OP_DIV) EXP EXP OP_CP
(defun $EXP1 (tokens)
    ;; (format t "EXP1.tokens: ~s~%" tokens) 
    (let ((result nil) (v1 nil) (v2 nil) (op nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXP1 (interp-result nil tokens))) 
        ;; 1
        (if (is-exp-op (next-token tokens)) 
            (progn
                (setq op (next-token tokens))
                (setq tokens (cdr tokens)))
            (return-from $EXP1 (interp-result nil tokens)))
        ;; 2
        (setq result ($EXP tokens))
        (if (car result)
            (progn 
                (setq v1 (car result))
                (setq tokens (cadr result)))
            (return-from $EXP1 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXP tokens))
        (if (car result) 
            (progn 
                (setq v2 (car result))
                (setq tokens (cadr result)))
            (return-from $EXP1 (interp-result nil tokens)))
        ;; 4
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result (eval-valuef op v1 v2) tokens)) 
            (return-from $EXP1 (interp-result nil tokens)))
        result))
        
(defun eval-expb (op v1 &optional (v2 nil))
    (setq v1 (if (string= "true" v1) t nil)) 
    (setq v2 (if (string= "true" v2) t nil))
    (cond
        ((string= op "KW_AND") (and v1 v2))
        ((string= op "KW_OR") (or v1 v2))
        ((string= op "KW_NOT") (not v1))))

(defun eval-valuef (op v1 v2)
    (if (not (numberp v1)) (setf v1 (get-sym *sym-tab* v1)))
    (if (not (numberp v2)) (setf v2 (get-sym *sym-tab* v2)))
    (cond
        ((string= op "OP_PLUS") (+ v1 v2))
        ((string= op "OP_MINUS") (- v1 v2))
        ((string= op "OP_DIV") (/ v1 v2))
        ((string= op "OP_MULT") (* v1 v2))
        ((string= op "OP_EQ") (= v1 v2))
        ((string= op "OP_GT") (> v1 v2))))

;; EXP := OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP 
(defun $EXP2 (tokens)
    ;; (format t "EXP2.tokens: ~s~%" tokens) 
    (let ((result nil) (branch nil) (v1 nil) (v2 nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXP2 (interp-result nil tokens))) 
        ;; 1
        (if (string= (next-token tokens) "KW_IF")
            (setq tokens (cdr tokens)) 
            (return-from $EXP2 (interp-result nil tokens))) 
        ;; 2
        (setq result ($EXPB tokens))
        (if (car result)
            (progn 
                (setq branch (string= (car result) "true"))
                (setq tokens (cadr result)))
            (return-from $EXP2 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXPLIST tokens))
        (if (car result) 
            (progn 
                (setq v1 (car result))
                (setq tokens (cadr result)))
            (return-from $EXP2 (interp-result nil tokens)))
        ;; 4
        (setq result ($EXPLIST tokens))
        (if (car result) 
            (progn 
                (setq v2 (car result))
                (setq tokens (cadr result)))
            (return-from $EXP2 (interp-result nil tokens)))
        ;; 5
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result (if branch v1 v2) tokens)) 
            (return-from $EXP2 (interp-result nil tokens)))
        result))

;; EXP := OP_OP KW_WHILE EXPB EXPLIST OP_CP 
(defun $EXP3 (tokens &optional (i 0))
    ;; (format t "EXP3.tokens: ~s~%" tokens) 
    (let ((result nil) (branch nil) (v 0) (jumped nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXP3 (interp-result nil tokens))) 
        ;; 1
        (if (string= (next-token tokens) "KW_WHILE")
            (setq tokens (cdr tokens)) 
            (return-from $EXP3 (interp-result nil tokens))) 
        ;; 2
        (setq result ($EXPB tokens))
        (if (car result)
            (progn 
                (setq branch (string= (car result) "true"))
                (setq tokens (cadr result)))
            (return-from $EXP3 (interp-result nil tokens)))
        ;; 3
        (if branch
            (progn 
                (setq result ($EXPLIST tokens))
                (if (car result) 
                    (progn 
                        (setq v (car result))
                        (setq tokens (cadr result)))
                    (return-from $EXP3 (interp-result nil tokens))))
            (progn
                (setq jumped (jump-next-exp tokens))
                (if jumped (setq tokens jumped) (return-from $EXP3 (interp-result nil tokens)))))
        ;; 4
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result v tokens)) 
            (return-from $EXP3 (interp-result nil tokens)))
        result))
    
;; OP_OP (DEFV | OP_SET) ID EXP OP_CP
(defun $EXP4 (tokens &optional)
    ;; (format t "EXP4.tokens: ~s~%" tokens) 
    (let ((result nil) (op nil) (id nil) (v nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXP4 (interp-result nil tokens))) 
        ;; 1
        (if (or (string= (next-token tokens) "KW_DEFV") (string= (next-token tokens) "KW_SET"))
            (progn 
                (setf op (next-token tokens))
                (setq tokens (cdr tokens)))
            (return-from $EXP4 (interp-result nil tokens))) 
        ;; 2
        (setq result ($ID tokens))
        (if (car result)
            (progn
                (setq id (car result)) 
                (setq tokens (cadr result)))
            (return-from $EXP4 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXP tokens))
        (if (car result) 
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $EXP4 (interp-result nil tokens)))
        ;; 4
        (if (string= (next-token tokens) "OP_CP") 
            (progn 
                (if (string= op "KW_DEFV")
                    (progn 
                        (setf *sym-tab* (def-sym *sym-tab* id *sym-type-var* v)))
                    (progn
                        (set-sym *sym-tab* id v)))
                (setq result (interp-result v tokens))) 
            (return-from $EXP4 (interp-result nil tokens)))
        result))

(defun $ID (tokens)
    ;; (format t "ID.tokens: ~s~%" tokens) 
    (if (string= (next-token tokens) "IDENTIFIER")
        (interp-result (next-value tokens) tokens) (interp-result nil tokens)))
         
(defun $VALUEF (tokens)
    ;; (format t "VALUEF.tokens: ~s~%" tokens) 
    (let ((token (next-token tokens)) (v nil))
        (if (string= token "VALUEF") 
            (setq v (next-value tokens)))
        (interp-result v tokens)))
    
(defun $EXPLIST (tokens)
    (let ((result))
        (cond 
            ((car (setq result ($EXPLIST1 tokens))) result)
            ((car (setq result ($EXPLIST2 tokens))) result))))

;; EXPLIST := OP_OP EXP OP_CP
(defun $EXPLIST1 (tokens)
    ;; (format t "EXPLIST1.tokens: ~s~%" tokens)
    (let ((result nil) (v nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXPLIST1 (interp-result nil tokens))) 
        ;; 1
        (setq result ($EXP tokens))
        (if (car result)
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $EXPLIST1 (interp-result nil tokens)))
        ;; 2
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result v tokens)) 
            (return-from $EXPLIST1 (interp-result nil tokens)))
        result))

;; EXPLIST := OP_OP EXPLIST OP_CP
(defun $EXPLIST2 (tokens)
    ;; (format t "EXPLIST2.tokens: ~s~%" tokens) 
    (let ((result nil) (v nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXPLIST2 (interp-result nil tokens))) 
        ;; 1
        (setq result ($EXPLIST tokens))
        (if (car result)
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $EXPLIST2 (interp-result nil tokens)))
        ;; 2
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result v tokens)) 
            (return-from $EXPLIST2 (interp-result nil tokens)))
        result))

(defun $EXPB (tokens)
    (let ((result))
        (cond 
            ((car (setq result ($TRUE tokens))) result)
            ((car (setq result ($FALSE tokens))) result)
            ((car (setq result ($EXPB1 tokens))) result)
            ((car (setq result ($EXPB2 tokens))) result)
            ((car (setq result ($EXPB3 tokens))) result))))

;; KW_TRUE           
(defun $TRUE (tokens)
    ;; (format t "TRUE.tokens: ~s~%" tokens) 
    (if (string= (next-token tokens) "KW_TRUE")
        (interp-result "true" tokens) (interp-result nil tokens)))

;; KW_FALSE         
(defun $FALSE (tokens)
    ;; (format t "FALSE.tokens: ~s~%" tokens) 
    (if (string= (next-token tokens) "KW_FALSE")
        (interp-result "false" tokens) (interp-result nil tokens)))

;; OP_OP (OP_EQ | OP_GT) EXP EXP OP_CP         
(defun $EXPB1 (tokens &optional (i 0))
    ;; (format t "EXPB1.tokens: ~s~%" tokens) 
    (let ((result nil) (op nil) (v1 nil) (v2 nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXPB1 (interp-result nil tokens))) 
        ;; 1
        (if (is-equality-op (next-token tokens))
            (progn 
                (setq op (next-token tokens)) 
                (setq tokens (cdr tokens)))
            (return-from $EXPB1 (interp-result nil tokens))) 
        ;; 2
        (setq result ($EXP tokens))
        (if (car result)
            (progn 
                (setq v1 (car result))
                (setq tokens (cadr result)))
            (return-from $EXPB1 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXP tokens))
        (if (car result)
            (progn 
                (setq v2 (car result))
                (setq tokens (cadr result)))
            (return-from $EXPB1 (interp-result nil tokens)))
        ;; 4
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result (convert-boolean (eval-valuef op v1 v2)) tokens)) 
            (return-from $EXPB1 (interp-result nil tokens)))
        result))

;; OP_OP (OP_AND | OP_OR) EXPB EXPB OP_CP
(defun $EXPB2 (tokens &optional (i 0))
    ;; (format t "EXPB2.tokens: ~s~%" tokens) 
    (let ((result nil) (op nil) (v1 nil) (v2 nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXPB2 (interp-result nil tokens))) 
        ;; 1
        (if (is-logical-op (next-token tokens))
            (progn 
                (setq op (next-token tokens))
                (setq tokens (cdr tokens)))
            (return-from $EXPB2 (interp-result nil tokens))) 
        ;; 2
        (setq result ($EXPB tokens))
        (if (car result)
            (progn 
                (setq v1 (car result))
                (setq tokens (cadr result)))
            (return-from $EXPB2 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXPB tokens))
        (if (car result)
            (progn 
                (setq v2 (car result))
                (setq tokens (cadr result)))
            (return-from $EXPB2 (interp-result nil tokens)))
        ;; 4
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result (convert-boolean (eval-expb op v1 v2)) tokens))
            (return-from $EXPB2 (interp-result nil tokens)))
        result))

;; OP_OP OP_NOT EXPB OP_CP           
(defun $EXPB3 (tokens &optional (i 0))
    ;; (format t "EXPB2.tokens: ~s~%" tokens) 
    (let ((result nil) (op "KW_NOT") (v nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $EXPB3 (interp-result nil tokens))) 
        ;; 1
        (if(string= (next-token tokens) "KW_NOT")
            (setq tokens (cdr tokens)) 
            (return-from $EXPB3 (interp-result nil tokens))) 
        ;; 2
        (setq result ($EXPB tokens))
        (if (car result)
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $EXPB3 (interp-result nil tokens)))
        ;; 3
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result (convert-boolean (eval-expb op v)) tokens)) 
            (return-from $EXPB3 (interp-result nil tokens)))
        result))

;; FUNCTION := OP_OP KW_DEFF ID OP_OP (ID | ID ID | ID ID ID) OP_CP EXPLIST OP_CP
(defun $FUNCTION (tokens)
    ;; (format t "FUNCTION.tokens: ~s~%" tokens) 
    (let ((result nil) (v nil))
        ;; 0
        (if (string= (next-token tokens) "OP_OP")
            (setq tokens (cdr tokens)) 
            (return-from $FUNCTION (interp-result nil tokens))) 
        ;; 1
        (if(string= (next-token tokens) "KW_DEFF")
            (setq tokens (cdr tokens)) 
            (return-from $FUNCTION (interp-result nil tokens))) 
        ;; 2
        (setq result ($ID tokens))
        (if (car result)
            (progn
                (setq id (car result)) 
                (setq tokens (cadr result)))
            (return-from $EXP4 (interp-result nil tokens)))
        ;; 3
        (setq result ($EXPLIST tokens))
        (if (car result) 
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $FUNCTION (interp-result nil tokens)))
        ;; 4
        (setq result ($EXPLIST tokens))
        (if (car result) 
            (progn 
                (setq v (car result))
                (setq tokens (cadr result)))
            (return-from $FUNCTION (interp-result nil tokens)))
        ;; 5
        (if (string= (next-token tokens) "OP_CP") 
            (setq result (interp-result "1f0" tokens)) 
            (return-from $FUNCTION (interp-result nil tokens)))
        result))

(defun $ARGS (tokens))


;; FCALL := OP_OP ID (EXP | EXP EXP | EXP EXP EXP) OP_CP
(defun $FCALL (tokens))

(defun jump-next-exp (tokens &optional (i 0))
    ;; (format t "consume-exp.i: ~d, ~s: ~%" i (next-token tokens)) 
    (cond 
        ((null tokens) nil)
        ((string= (next-token tokens) "OP_OP") (jump-next-exp (cdr tokens) (1+ i)))
        ((string= (next-token tokens) "OP_CP") 
            (if (= i 1)
                (cdr tokens) 
                (jump-next-exp (cdr tokens) (1- i))))
        ((= i 0) (cdr tokens))
        (t (jump-next-exp (cdr tokens) i))))

(defun convert-boolean (bool)
    (if bool "true" "false"))

(defun is-logical-op (str)
    (or (string= str "KW_AND") (string= str "KW_OR")))

(defun is-equality-op (str)
    (or (string= str "OP_EQ") (string= str "OP_GT")))

(defun is-exp-op (str)
    (or (string= str "OP_PLUS") (string= str "OP_MINUS") (string= str "OP_MULT") (string= str "OP_DIV")))

(defun interp-result (ok token-list)
    (if ok (cons ok (list (cdr token-list))) (cons ok (list token-list))))

(defun next-token (token-list) 
    (cadar token-list))

(defun next-value (token-list)
    (caar token-list))

(defun print-error (&optional e)
    (case
        0 (format t "ERROR Use of undecleared variable ~s.~%" v)))

(defun format-valuef (v)
    (format nil "~sf~s" (numerator v) (denominator v)))

(gppinterpreter *args*)
(print-sym-tab *sym-tab*)