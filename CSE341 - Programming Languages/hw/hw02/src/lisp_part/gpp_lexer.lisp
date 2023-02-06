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

;;; lexical error types
(defconstant LEADING_DIGIT "LEXICAL_ERROR: LEADING_DIGIT")
(defconstant LEADING_ZERO "LEXICAL_ERROR: LEADING_ZERO")
(defconstant SPELLING_ERROR "LEXICAL_ERROR: SPELLING_ERROR")
(defconstant ILLEGAL_CHARACHTER "LEXICAL_ERROR: APPERANCE_OF_ILLEGAL_CHARACHTER")

;;; flags
(setf comment-flag nil)
(setf string-flag nil)
(setf exit-flag nil)

(setf token-list ())        ;; list of tokens with associated token clases
(setf unmached-token "")    ;; unmached token during tokenization process

(defun gppinterpreter (&optional filename)
    (if filename
        (progn
            (lexer-from-file filename)
            (print token-list)        
        )
        (lexer-from-cl)
    )
    token-list ;; return token list
)

;;; lexer function that make lexical analysis for the provided input file 
(defun lexer-from-file (filename)
    ;; read the file char by char and apply tokenization
    (with-open-file (stream filename) 
        (loop :for curr-char := (read-char stream nil) :while curr-char :do
            (tokenize curr-char))
    )
    (setf token-list (reverse token-list)) ;; reverse the token list and return it
)

;;; lexer function that make lexical analysis for the user command line input
(defun lexer-from-cl ()
    (format t "> ") 
    ;; read the file char by char and apply tokenization
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
    (cond
        ;;; COMMENT
        (comment-flag 
            ;; consume all the characters till the end of line
            (if (char= curr-char #\Newline)
                (progn
                    (setf comment-flag nil)
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
            ;; consume all the characters till the quote (\") charachter
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
                            (if (string= unmached-token "exit") 
                                (setf exit-flag t)) ;; flag is up to terminate execution
                            (push result token-list))
                        ;; OPERATOR
                        ((setf result (map-operator unmached-token))
                            (push result token-list))
                        ;; LITERAL (VALUEI or VALUEF)
                        ((is-valuei unmached-token)
                            (push (list unmached-token "VALUEI") token-list))
                        ((is-valuef unmached-token)
                            (push (list unmached-token "VALUEF") token-list))
                        ;; IDENTIFIER
                        ((is-identifier unmached-token)
                            (push (list unmached-token "IDENTIFIER") token-list))
                        ;; SYNTAX-ERROR
                        (t
                            (push (list unmached-token (specify-error unmached-token)) token-list))
                    )                        
                    (setf unmached-token "") ;; reset unmached token
                )
            )
            (if (or (string= curr-char "(") (string= curr-char ")"))
                (push (map-operator curr-char) token-list))
        )

        ;;; UNMACHED TOKEN
        (t
            (setf unmached-token (concat-end unmached-token curr-char))
        )
    )          
)

(defun specify-error (sample)
    (setf first-char (char sample 0))
    (cond 
        ;; leading zero (012)
        ((char= first-char #\0)
            LEADING_ZERO)
        ;; leading digit (1_var)
        ((is-digit first-char)
            LEADING_DIGIT)
        ;; illegal syntax (+3a)
        ((is-legal-token sample)
            SPELLING_ERROR)
        ;; apperance of illegal charachter (my$variable)
        (t
            ILLEGAL_CHARACHTER)
    )
)

;;; checks if the given token consist of legal charachters
(defun is-legal-token (sample)
    (is-legal-token-helper sample 0 (length sample)) 
)

(defun is-legal-token-helper (sample curr end)
    (if (eq curr end)
        t
        (progn 
            (setf c (char sample curr))
            ;; check if current char is legal charachter
            (if (or (is-digit c) (is-alpha c) (char= c #\_) (map-operator c))
                (is-legal-token-helper sample (1+ curr) end)
                nil
            )
        )
    )
)

(defun is-leading-zero (sample)
    (setf first-char (char sample 0))
    (if (char= first-char #\0)
        (if (eq (length sample) 1) 
            nil ;; value is 0
            t   ;; lexical error: LEADING_ZERO or LEADING_DIGIT
        )
        nil
    )
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
    (if (is-leading-zero sample)
        nil ;; leading zero error
        (is-valuei-helper sample 0 (length sample))
    )
)

(defun is-valuei-helper (sample curr end)
    (cond 
        ((eq curr end)
            t)
        ((is-digit (char sample curr))
            (is-valuei-helper sample (1+ curr) end))
        (t
            nil)
    )
)

(defun is-valuef (sample)
    (if (is-leading-zero sample)
        nil ;; leading zero error
        (is-valuef-helper sample 0 (length sample) nil))
)

(defun is-valuef-helper (sample curr end valueff)
    (cond 
        ((eq curr end) 
            t)
        ((is-digit (char sample curr))
            (is-valuef-helper sample (1+ curr) end valueff))
        (valueff 
            nil)
        ((char= (char sample curr) #\f) ;; set valuef flag true and continue analysis
            (is-valuef-helper sample (1+ curr) end t))
        (t
            nil)
    )
)

;;; checks if the given charachter terminates the tokenization process 
(defun is-terminatorc (sample)
    (or
        (char= sample #\Space)
        (char= sample #\Newline)
        (char= sample #\Tab)
        (string= sample "(")
        (string= sample ")"))
)

(defun is-identifier (sample)
    ;; first letter should not contain digit ([a-zA-Z_])
    ;; rest can be alpanumeric ([a-zA-Z0-9_]*)
    (setf firstLetter (char sample 0))
    (if (or (is-alpha firstLetter) (char= firstLetter #\_))
        (is-identifier-helper sample 1 (length sample))
        nil
    )
)

(defun is-identifier-helper (sample curr end)
    (if (eq curr end) 
        t
        (progn
            (setf c (char sample curr))
            (if (or (is-alpha c) (is-digit c) (char= c #\_))
                (is-identifier-helper sample (1+ curr) end)
                nil
            )
        )
    )
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