
(in-package :climwi)


;; So, here we define stream-accept for strings to use the simple parser...



;; normally this would make a string input stream
(defmethod stream-accept ((stream string) type
                          &key
                            error-if-not-eof view
                            delimiter-gestures)
  (execute-parser (lambda ()
                    (accept type :stream :simple-parser
                            :view view
                            :delimiter-gestures delimiter-gestures))
                  stream
                  :allow-trailing-junk (not error-if-not-eof)))

;; (accept 'integer :stream "12")

;; in case we get strange results from the parser
;; (eg: consuming characters but not returning a true value in some node)
;; this allows us to trace the whole execution of it.
(defparameter *debug-parser* nil)

(defmethod stream-accept ((stream (eql :simple-parser)) type
                          &key view delimiter-gestures)
  
  (when *debug-parser*
    (format *standard-output*
            "<PARSER ENTRY>: ~S ~% ~S~%========================~%"
            type
            (subseq simple-parser:*parser-input-string*
                    simple-parser:*parser-input-position*)))
  
  (let ((result (let ((climwi::*delimiter-gestures* delimiter-gestures))
                  (climwi::funcall-presentation-generic-function
                   accept type :simple-parser view))))
    (when *debug-parser*
      (format *standard-output*
              "<PARSER RESULT>: ~S ~% ~S~%========================~%"
              type
              result))
    
    result))


(defmacro define-parser-acceptor (type &body body)
  `(define-presentation-method accept (,(if (consp type)
                                            type
                                            `(,(gensym "type") ,type))
                                        (stream (eql :simple-parser)) view &key error-if-not-eof)
     ;; these are just here to suppress unused variable warnings
     error-if-not-eof view
     ,@body))


;; unless we create something to accept a thing then the thing is just not acceptable...
(define-parser-acceptor thing
  nil)


;; Whilst this may seem pointless, it is provided so that :around methods can do things based on metaclass etc
;; (see cldb.lisp)
(define-parser-acceptor t
  (error 'ccl::no-applicable-method-exists :gf #'climwi::%accept
                                           :args (list climwi::type-key stream climwi::view
                                                       climwi::error-if-not-eof)))

;; !!! Need a lot more of these
(define-parser-acceptor integer
  (awhen (sp-scan "^-?\\d+")
    (parse-integer it)))

;; (accept-from-string 'integer "3")


;; 


;; we can easily do things like this...
(define-parser-acceptor (type list)
  (let ((first (accept climwi::?element-type
                       :stream stream :view view
                       :delimiter-gestures (or (typespec-option type :delimiter-gestures)
                                               (cond ((equal separator ", ")
                                                      (list #\, #\Space))
                                                     (t (error "Can't figure out :delimiter-gestures!")))))))
    (if first
        (aif (sp-scan separator-regex)
             (cons first
                   (accept type :stream stream :view view))
             (list first))
        ;; There should be something for describing presentation types
        ;; which looks at the label where supplied and 
        ;; (sp-error "Expected ~A here: " climwi::?element-type)
        )))

;; This gives us considerable flexibility to do these sorts of things:-
;; (accept-from-string '((list integer) :separator-regex "^\\s+") "1  2 3")
;; (accept-from-string '((list integer) :separator-regex "^\\s+") "1, 2, 3")
;; (accept-from-string '((list integer)) "1, 2, 3")
;; (accept-from-string '(list integer) "asd")


;; !!! I could define a shortcut for these like I had before to eliminate some of the
;; repetetive arguments
(define-parser-acceptor currency
  (awhen (sp-scan "^(?:[^\\d]?-?|\\-[^\\d])[\\d\\,]*\\d+(\\.\\d+)?"
                  ;; The following is more specific about thousand separators:-
                  ;; I'm not enabling it for the moment though
                  ;; "^[^\\d]?-?(?:\\d\\d?\\d?(,\\d\\d\\d)+|\\d+)(\\.\\d+)?"
                  )
    (parse-rational (regex-replace-all "," (regex-replace "^[^\\d\\-]" it "") ""))))

;; (accept-from-string 'currency "12.3")


(define-parser-acceptor percentage
  (awhen (or (sp-scan "^-?\\d+(\\.\\d+)?%")
             (and (not require-percent-sign)
                  (sp-scan "^-?\\d+(\\.\\d+)?")))
    (parse-rational (regex-replace "\\%" it ""))))

;; (accept-from-string 'percentage "12%")
;; (accept-from-string 'percentage "-12.2%")
;; (accept-from-string 'percentage "-12.2")

(define-parser-acceptor string
  (flet ((unescape (string)
           (regex-replace-all "\\\\([^\\\\])" string "\\1"))
         (read-to-delimiter (delimiters)
           (sp-scan (format nil "^(?:\\\\.|[^~A])*"
                            (quote-meta-chars (map 'string #'identity delimiters))))))

    ;; only when the delimiter gestures are :eof do we NOT do the escapes. With space as a delimiter gesture we can use \ to escape a space
    (cond ((eq climwi::*delimiter-gestures* :eof)
           (sp-scan "^(?:.|\\s)+"))
          ;; if we get a single or double quote then read up to the close one...
          ((sp-next-char-is #\')
           (sp-skip-char)
           (let ((string (read-to-delimiter (list #\'))))
             (or (sp-next-char-is #\')
                 (sp-error "Expected '"))
             (sp-skip-char)
             (unescape string)))
          
          ((sp-next-char-is #\")
           (sp-skip-char)
           (let ((string (read-to-delimiter (list #\"))))
             (or (sp-next-char-is #\")
                 (sp-error "Expected \""))
             (sp-skip-char)
             (unescape string)))

          ;; otherwise we presume that there is some set of delimiters, which are characters, up to which we will read, but we don't mind if we don't find those delimiters and in this case we will read to EOF
          (climwi::*delimiter-gestures*
           (let ((string (read-to-delimiter climwi::*delimiter-gestures*)))
             (when (and string
                        (not (equal string "")))
               (unescape string))))

          (t (error "Can't read string without any delimiters specified (what should happen here?)")))))


;; This just accepts everything as the string (up to eof)
;; (accept-from-string 'string "hi there")
;; (accept-from-string 'string "") ; yields nil. Is this right?

;; (accept-from-string 'string "'hello there'" :delimiter-gestures (list #\Space))
;; (accept-from-string 'string "'hello there" :delimiter-gestures (list #\Space))
;; (accept-from-string 'string "'hello ' there" :delimiter-gestures (list #\Space))

;; (accept-from-string 'string "'Why can\\'t I put in single quotes'" :delimiter-gestures (list #\Space))
;; (accept-from-string 'string "'Why can\\\\t I put in single quotes'" :delimiter-gestures (list #\Space))

;; (accept-from-string 'string "\"hi \\\"the\\\\re\"" :delimiter-gestures (list #\Space))

;; !!! Along with this I need to know how to present a string completely acceptably. This will depend on the expected delimiters though. Can we control this with view and/or acceptably?
;; in minput fields we can present without acceptably. Otherwise we should probably present acceptably (especially for commands) which will have to quote things if any special characters (spaces, commas, quotes) appear
;; AND it will have to escape things, for which I need a function

;; (accept-from-string 'string "hi there" :delimiter-gestures (list #\Space))
;; (accept-from-string 'string "hi\\ there" :delimiter-gestures (list #\Space))





(define-parser-acceptor or
  (when climwi::??c
    (or (accept (first climwi::??c) :stream :simple-parser)
        (accept `(or ,@(cdr climwi::??c)) :stream :simple-parser))))

;; If we didn't have sp-try we could just have this error when the accepted value isn't right
;; that might be better really
(define-parser-acceptor (type and)
  (sp-try (lambda ()
            (awhen (accept (first climwi::??c) :stream :simple-parser)
              (when (presentation-typep it type)
                it)))))

;; (presentation-typep 12 '(and integer (satisfies evenp)))

;; (accept-from-string '(and integer (satisfies evenp)) "5")
;; (accept-from-string '(or (and integer (satisfies evenp)) currency) "5.24")




;; I'm not sure that this will have any meaningful failure behaviour
;; !!! What about subcommands marked with parens? Should I bother with those?
;; I could do I suppose
;; (see previous read-command-parameter, though it's trivial)

(define-parser-acceptor parameter-list
  ;; when to error and when to just return nil?
  ;; Also, whose responsility is it to check the types of the parameters?
  (labels ((read-parameters (p &key first first-required)
             (unless first
               (sp-scan separator-regex))
             (when p
               (if (and (not first-required)
                        (sp-scan "^\\#\\?"))
                   ;; generate a defaulted incomplete marker
                   (let ((r (read-parameters p :first t :first-required t)))
                     (cons (list climwi::*incomplete-command-marker*
                                 (with-presentation-type-decoded (name params opts)
                                     (first p)
                                   `((,name ,@params) :default ,(first r) ,@opts)))
                           (cdr r)))
                   (let ((x (accept (first p)
                                    :stream :simple-parser
                                    :delimiter-gestures (list #\Space))))
                     (when first-required
                       (unless (presentation-typep x (first p))
                         (sp-error "Expected ~A" (first p))))
                     ;; !!! Check all this logic carefully
                     (cond ((presentation-typep x (first p))
                            (cons x (read-parameters (cdr p))))
                           ((and (sp-eof)
                                 incomplete-marker)
                            (cons (list climwi::*incomplete-command-marker* (first p))
                                  (read-parameters (cdr p))))
                           ((and expected incomplete-marker)
                            (list climwi::*incomplete-command-marker*))
                           ((and first (not expected))
                            nil)        ; fail the whole thing
                           (t (sp-error "Expected parameter of type ~S here" (first p)))))))))
    (sp-skip-whitespace)
    (read-parameters ??parameters :first t)))
;; if we hit eof with more parameters pending we can return an 'incomplete' marker if given in the options

;; (accept-from-string '(parameter-list integer integer) "23 94")
;; (accept-from-string '(parameter-list integer integer) "")

;; (presentation-typep nil '(eql nil))
;; (accept-from-string '(parameter-list (or integer (eql nil))) "")


;; this isn't very flexible or general. I wonder if I can use the format option instead.
(define-parser-acceptor date-and-time
  (awhen (sp-scan "^\\d\\d?/\\d\\d?/\\d\\d(?:\\d\\d)?\\s*\\-\\s*\\d\\d\\:\\d\\d(?:\\:\\d\\d)?")
    (cybertiggyr-time:parse-time (regex-replace-all "\\s" it "")
                                 (list (cybertiggyr-time::make-fmt-recognizer "%d/%m/%Y-%H:%M:%S")
                                       (cybertiggyr-time::make-fmt-recognizer "%d/%m/%Y-%H:%M")))))
;; (present-to-string (accept-from-string 'date-and-time "1/2/2016 - 12:23") 'date-and-time)




;; This could be made very general but I think I will 
(define-parser-acceptor date
  (or (awhen (sp-scan "^\\d\\d?/\\d\\d?/\\d\\d(?:\\d\\d)?")
        (if (equal it "01/01/1900")
            0
            (parse-date it)))
      (awhen (sp-scan "^\\d\\d\\d\\d-\\d\\d?-\\d\\d?")
        (parse-date (formatted-date (cybertiggyr-time:parse-time it))))))

;; (present-to-string (accept-from-string 'date "3/02/2016") 'date-and-time)



;; What about false? Well, we can't FAIL to accept that. That's sort of a problem
(define-parser-acceptor boolean
  (cond ((or (sp-scan "^true$" :ignore-case t)
             (sp-scan "^yes$" :ignore-case t))
         t)
        ((or (sp-scan "^false$" :ignore-case t)
             (sp-scan "^no$" :ignore-case t))
         ;; because we're returning nil we have to indicate that we didn't just fail...
         ;; (this isn't implemented yet)
         (values nil t))))


;; (accept-from-string 'boolean "true")
;; (accept-from-string 'boolean "asd")

;; this is quite problematic and I'm not really sure what to do. Maybe we need to use a second value
;; to indicate that we have READ a nil value and accepted it


(define-parser-acceptor eql
  (when (sp-scan (s "^~A" object) :ignore-case t)
    object))




;; I'm going to make this a little bit slacker than the postcode
;; regex, but it will always yield the normalised postcode...

(define-parser-acceptor uk-postcode
  (awhen (sp-scan "^[A-Z]{1,2}[0-9R][0-9A-Z]? *[0-9][ABD-HJLNP-UW-Z]{2}"
                  :ignore-case t)
    (regex-replace "([^\\s])(\\d[A-Z]{2})$"
                   (string-upcase it)
                   "\\1 \\2")))

;; (accept-from-string 'uk-postcode "TQ124nr")
;; (accept-from-string 'uk-postcode "TQ 124nr")

(define-parser-acceptor member
  (accept (cons 'or
                (mapcar (lambda (object)
                          (list 'eql object))
                        objects))
          :stream :simple-parser))


(define-parser-acceptor description
  (accept 'string :stream :simple-parser))



;; This returns a typespec
(define-parser-acceptor numeric-range
  (or (awhen (sp-scan "^[\\d]+\\s*--?\\s*[\\d]+")
        (cons 'integer
              (mapcar #'parse-integer
                      (split "\\s*-+\\s*" it))))
      (awhen (sp-scan "^\\d+\\+")
        (list 'integer (parse-integer (regex-replace "\\+" it ""))))))

;; (accept-from-string 'numeric-range "1-34")
;; (accept-from-string 'numeric-range "100+")
