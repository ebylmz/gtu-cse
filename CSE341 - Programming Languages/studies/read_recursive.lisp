;; CL standart input file: *standard-input*
;; CL standart output file: *standard-output*

(defvar *input-file-name* "input.txt")
(defvar *output-file-name* "output.txt")

(defun read-recursive (stream-in stream-out)
    (let ((ch (read stream-in nil)))
        (unless (null ch)
            (format stream-out "~s~%" ch)
            (read-recursive stream-in stream-out))))

(let ((file-in (open *input-file-name* :direction :input)) (file-out (open *output-file-name* :direction :output)))
    (read-recursive file-in file-out))

;; with-open-file closes file automaticly
(print (with-open-file (file-stream *input-file-name*)
    (with-output-to-string (string-stream)
        (read-recursive file-stream string-stream) string-stream)))