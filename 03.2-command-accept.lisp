
(in-package :climwi)


;; Accept commands
;; I've pulled this out into here since it's using the SP framework to do its thing
;; (although accept also depends on this, so things are getting a bit intertwined)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Let's start parsing command strings...

;; !!! This skips over whitespace so there must be some non whitespace values remaining
;; (defun eof-p (stream)
;;   (eq (peek-char t stream nil :eof) :eof))

(defun accept-named-command-table-entry (table parameters return-type)
  (sp-skip-whitespace)
  (let ((commands-to-search (sort (find-commands-in-table
                                   ;; filter the command table...
                                   (if (and (eq parameters 'parameter-list)
                                            (eq return-type t))
                                       (constantly t)
                                       (lambda (entry)
                                         ;; check for compatible types
                                         ;; !!! see defun accept for how to fix this
                                         (and (presentation-subtypep (command-return-type entry)
                                                                     return-type)
                                              (presentation-subtypep (make-parameter-list
                                                                      (command-parameters entry))
                                                                     parameters))))
                                   (resolve-command-table table))
                                  #'>
                                  :key (@ length (command-name ?1)))))
    (or (loop for entry in commands-to-search
           ;; !!! Should I strip out the whitespace here?
           when (or (sp-starts-with-string (command-name entry))
                    (sp-starts-with-symbol (command-symbol-name entry)))
           ;; then we can assume that this is what is intended...
           return entry)
        (sp-try (lambda ()
                  (awhen (sp-scan "^[a-zA-Z\\s]+")
                    (let ((test (cl-ppcre:create-scanner (s "^~A" (regex-replace " $"
                                                                                 (regex-replace-all "\\s+"
                                                                                                    it
                                                                                                    "[^\\s]* ")
                                                                                 ""))
                                                         :case-insensitive-mode t)))
                      (awhen (remove-if-not (lambda (e)
                                              (scan test (command-name e)))
                                            commands-to-search)
                        (unless (cdr it)
                          (first it))))))))))


(defun accept-json-command (table parameters return-type)
  (declare (ignore parameters return-type))
  ;; - we will get a type error form elsewhere if it's not right
  (with-input-from-string (stream (subseq *parser-input-string* *parser-input-position*))
    (awhen (json:decode-json stream)
      (incf *parser-input-position* (file-position stream))
      (let ((cte (find-command (first it) (resolve-command-table table))))
        (add-incomplete-markers
         (cons (command-symbol-name cte)
               (with-presentation-type-decoded (name params)
                   (climwi::command-parameters-type cte)
                 (declare (ignore name))
                 (loop for param in params
                    for arg in (cdr it)
                    collect (if (stringp arg)
                                (accept-from-string param arg)
                                arg)))))))))


(define-parser-acceptor command
  (if (sp-next-char-is #\[)
      (accept-json-command table parameters return-type)
      (awhen (accept-named-command-table-entry table
                                               parameters
                                               return-type)
        (cons (command-symbol-name it)
              ;; !!! I should probably not do this accept if the parameter list is empty
              ;; it will always fail anyway, but it's not necessary
              (accept (command-parameters-type it
                                               :expected t
                                               :incomplete-marker t)
                      :stream :simple-parser)))))



(define-parser-acceptor command-with-unevaluated-parameters
  (if (sp-next-char-is #\[)
      (accept-json-command table parameters return-type)
      (awhen (accept-named-command-table-entry table
                                               parameters
                                               return-type)
        (cons (command-symbol-name it)
              (accept (unevaluated-parameter-list-type
                       (command-parameters-type it
                                                :expected t
                                                :incomplete-marker t))
                      :stream :simple-parser)))))




;; (accept *test-commands-2* :stream "Hello David")
;; (accept *test-commands-2* :stream "Hello David Ritchie")
;; (accept *test-commands-2* :stream "Hello 'David'")

;; deleted: (defun mcommands ...)
