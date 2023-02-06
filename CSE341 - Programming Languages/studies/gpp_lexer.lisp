;;; LEXER THAT CAN PARSE NON-SPACED TOKENS

;;; tokens and their token classes as an association list
(setf keywords-alist '(
    ("and" "KW_AND") 
    ("or" "KW_OR") 
    ("not" "KW_NOT") 
    ("equal" "KW_EQUAL") 
    ("less" "KW_LESS") 
    ("nil" "KW_NIL") 
    ("list" "KW_LIST")
    ("append" "KW_APPEND") 
    ("concat" "KW_CONCAT") 
    ("set" "KW_SET") 
    ("deffun" "KW_DEFFUN") 
    ("for" "KW_FOR") 
    ("if" "KW_IF")
    ("exit" "KW_EXIT") 
    ("load" "KW_LOAD") 
    ("disp" "KW_DISP") 
    ("true" "KW_TRUE") 
    ("false" "KW_FALSE")
))

(setf operators-alist '(
    ("+" "OP_PLUS")
    ("-" "OP_MINUS") 
    ("/" "OP_DIV") 
    ("**" "OP_DBLMULT")
    ("*" "OP_MULT") 
    ("(" "OP_OP") 
    (")" "OP_CP")
    ("\"" "OP_OC")
    ("\"" "OP_CC") 
    ("," "OP_COMMA")
))

;;; flags
(setf comment-flag nil)
(setf string-flag nil)
(setf operator-flag nil)
(setf value-flag nil)
(setf exit-flag nil)

(setf token-list ())        ;; list of tokens with associated token clases
(setf unmached-token "")    ;; unmached token during tokenization process

(defun gppinterpreter (&optional filename)
    (if filename
        (progn
            (lexer-from-file filename)
            ;; (loop :for token :in token-list :do (format t "~s~%" token))
            (print token-list)        
        )
        (lexer-from-cl)
    )
    token-list ;; return token list
)

(defun lexer-from-file (filename)
    ;; read the file char by char and apply tokenization
    (with-open-file (stream filename) 
        (loop :for curr-char := (read-char stream nil) :while curr-char :do
            (tokenize curr-char)
            ;; (if exit-flag (return))
        )
    )
    (setf token-list (reverse token-list)) ;; reverse the token list and return it
)

(defun lexer-from-cl ()
    (format t "> ") 
    (loop :for curr-char := (read-char nil) :do
        (tokenize curr-char)

        (cond 
            (exit-flag
                (format t "Bye~%")
                (return))
            ((char= curr-char #\Newline)
                (progn
                    (print (reverse token-list))
                    (format t "~%~%")
                    (setf token-list ()) ;; reset the token list
                    (format t "> ")
                ))
        )
    )
)

(defun tokenize (curr-char)
    ;; CASE: Comment
        ;; if the first time set the comment-flag up
        ;; consume all the characters till the end of line
    ;; CASE: String 
        ;; consume all the characters till the quote (\") charachter
    ;; CASE: Termination Operator
        ;; if there is an unmached token (> (length unnmachedToken) 0)
            ;; evaluate it (either valid or invalid token)
                ;; is keyword
                ;; is operator
                ;; is literal, which type (VALUEI (123) or VALUEF (123f456))
                ;; is identifier ([a-zA-Z_][a-zA-Z0-9_]*)
                ;; is lexical-error
            ;; reset the unmached-token
        ;; Add termination token to token list
    ;; CASE: Unmached Token
        ;; continue to building the token (concatenate)
    
    ;;; TOKENIZATION STOP CASES:
    ;; CASE: Termination charachters (#\Space #\Newline #\Tab "(" ")")
    ;; CASE: Operator (**)
    ;; CASE: previous char is operator and current char is not operator
    ;; CASE: previous char is value and current char is not value except f for fractional numbers

    (cond
        ;;; COMMENT
        (comment-flag 
            (if (char= curr-char #\Newline)
                (progn
                    (setf comment-flag nil)
                    ;; (format t "Comment: ~a~%" unmached-token) ;; DEBUG PURPOSES
                    (push (list unmached-token "COMMENT") token-list)
                    (setf unmached-token "") ;; reset unmached token
                )
                (setf unmached-token (concat-end unmached-token curr-char))
            )
        )

        ((char= curr-char #\;) 
            (if (string= unmached-token ";") 
                (progn 
                    (setf unmached-token "") ;; reset unmached token
                    (setf comment-flag t)
                )
                (setf unmached-token (concat-end unmached-token curr-char))
            )   
        )

        ;;; STRING
        ((and (eq string-flag nil) (char= curr-char #\"))
            (push (map-operator curr-char) token-list) 
            (setf string-flag t))
        (string-flag
            (cond 
                ((char= curr-char #\") ;; end of the string
                    (setf string-flag nil)
                    (push (list unmached-token "VALUESTR") token-list)
                    (push '("\"" "OP_CC") token-list)
                    (setf unmached-token "")) ;; reset unmached token
                    
                (t
                    (setf unmached-token (concat-end unmached-token curr-char)))   
            )  
        )

        ;;; TERMINATOR CHAR 
        ((is-terminatorc curr-char)
            (if (> (length unmached-token) 0)
                (progn 
                    (setf result nil)
                    (cond                        
                        ;; KEYWORD
                        ((setf result (map-keyword unmached-token)) 
                            (if (string= unmached-token "exit") ;; terminate execution
                                (setf exit-flag t))
                            (push result token-list))
                        ;; OPERATOR
                        ((setf result (map-operator unmached-token))
                            (push result token-list))
                        ;; LITERAL (VALUEI or VALUEF)
                        ((is-valuei unmached-token)
                            (push (list unmached-token "VALUEI") token-list)
                            (setf value-flag nil))
                        ((is-valuef unmached-token)
                            (push (list unmached-token "VALUEF") token-list)
                            (setf value-flag nil))
                        ;; IDENTIFIER
                        ((is-identifier unmached-token)
                            (push (list unmached-token "IDENTIFIER") token-list))
                        ;; SYNTAX-ERROR
                        (t
                            (push (list unmached-token "LEXICAL_ERROR") token-list))
                    )                        
                    (setf unmached-token "") ;; reset unmached token
                )
            )
            (if (or (string= curr-char "(") (string= curr-char ")")) ;;; WRITE MORE SIMPLE
                (push (map-operator curr-char) token-list))
        )
        ;;; OTHER SPECIAL TERMINATION CASES
        ;; if previous char is operator and current char is not operator
        ;; push the token with token class and set unmached-token as current char
        ((and operator-flag (not (map-operator curr-char)))
            (push (map-operator unmached-token) token-list)
            (setf unmached-token curr-char)
        )

        ;; if previous char is value and current char is not value except f for fractional numbers
        ;; push the token with token class and set unmached-token as current char
        ((and value-flag (not (is-digit curr-char)) (not (string= curr-char "f")))
            (cond 
                ;; LITERAL (VALUEI or VALUEF)
                ((is-valuei unmached-token)
                    (push (list unmached-token "VALUEI") token-list)
                    (setf value-flag nil))
                ((is-valuef unmached-token)
                    (push (list unmached-token "VALUEF") token-list)
                    (setf value-flag nil))
                (t
                    (push (list unmached-token "LEXICAL_ERROR") token-list))
            )
            (setf unmached-token (string curr-char))
            (setf value-flag nil)
        )

        ;;; OPERATOR 
        ((map-operator curr-char)
            ;; evaulate immediately the operators except double multiplication (**)  
            (if (string= curr-char "*")
                (progn 
                    (if operator-flag 
                        (progn ;; double multiplication
                            (push (map-operator "**") token-list)
                            (setf unmached-token "")
                            (setf operator-flag nil)
                        )
                        (progn
                            (setf operator-flag t)
                            ;; below not needed                
                            (setf unmached-token (concat-end unmached-token curr-char))
                        )
                    )
                )
                (push (map-operator curr-char) token-list)
            )
        )

        ;;; VALUE
        ((is-digit curr-char)
            (setf value-flag t)
            (setf unmached-token (concat-end unmached-token curr-char))
        )

        ;;; UNMACHED TOKEN
        (t
            (setf unmached-token (concat-end unmached-token curr-char))
        )
    )          
)

(defun is-leading-zero (sample)
    (setf first-char (char sample 0))
    (if (char= first-char #\0)
        (if (eq (length sample) 1) 
            (setf result nil) ;; value is 0
            (setf result t) ;; LEADING_ZERO or LEADING_DIGIT error
        )
        (setf result nil) ;; not leading zero
    )
    result
)

(defun is-digit (sample)
    (setf ascii (char-code sample))
    (and (>= ascii (char-code #\0)) (<= ascii (char-code #\9)))
)

(defun is-alpha (sample)
    (setf ascii (char-code sample))
    (or 
        (and (>= ascii (char-code #\a)) (<= ascii (char-code #\z))) 
        (and (>= ascii (char-code #\A)) (<= ascii (char-code #\Z))))
)

(defun is-valuei (sample)
    (setf result (not (is-leading-zero sample)))
    (loop :for c :across sample :when result :do
        ;; should only consist of digits
        (setf ascii (char-code c))
        (if (not (is-digit c))
        ;; (if (not (and (>= ascii (char-code #\0)) (<= ascii (char-code #\9))))
            (setf result nil)
        )
    )
    result
)

;;; checks if the given charachter is cause termination for the tokenization 
(defun is-terminatorc (sample)
    (or
        (char= sample #\Space)
        (char= sample #\Newline)
        (char= sample #\Tab)
        (string= sample "(")
        (string= sample ")"))
)

(defun is-valuef (sample)
    (setf result (not (is-leading-zero sample)))
    (setf fflag nil) ;; fraction flag
    (loop :for c :across sample :when result :do
        (cond 
            ((is-digit c)
                ())
            (fflag
                (setf result nil))
            ((char= c #\f)
                (setf fflag t))
            (t
                (setf result nil))    
        )
    )
    (and fflag result)
)

(defun is-identifier (sample)
    ;; first letter should not contain digit ([a-zA-Z_])
    (setf firstLetter (char sample 0))
    (setf result (or (is-alpha firstLetter) (char= firstLetter #\_)))
    
    ;; rest can be alpanumeric ([a-zA-Z0-9_]*)
    (setf rest (subseq sample 1 (length sample)))
    (loop :for c :across rest :when result :do
        (if (not (or (is-alpha c) (is-digit c) (char= c #\_)))
            (setf result nil))
    )
    result
)

;;; adds s2 to end of s1 and sets s1 the concatenated string
(defun concat-end (s1 s2)
    (concatenate 'string s1 (string s2))   
)

;;; check the operators
(defun map-operator (key)
    (assoc key operators-alist :test #'string=)
)

;;; check the keywords
(defun map-keyword (key)
    (assoc key keywords-alist :test #'string=)
)

;;; call the lexer with input file given as command line argument
(gppinterpreter (car EXT:*ARGS*))
