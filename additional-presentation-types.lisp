
(in-package :climwi)


;; Extra useful things...


;; !!! What about options? Needed?
(define-presentation-type scan (re)
  :inherit-from 'string)

(define-presentation-method presentation-typep (object (type scan))
  (and (stringp object)
       (scan re object) t))

;; (presentation-typep "hi there" '(scan "[a-z]"))
;; (presentation-typep "123" '(scan "[a-z]"))

(define-presentation-method type-for-subtype-tests ((type scan))
  'string)

;; This does not yet work because we haven't defined the and presentation type
;; (presentation-typep "hi there" '(and string (scan "[a-z]")))


(define-presentation-type one-of (&rest items)
  :inherit-from 'string)

(define-presentation-method presentation-typep (object (type one-of))
  (and (stringp object)
       (member object items :test #'equal)
       t))

;; (presentation-typep "hi" '(one-of "hi" "hello" "howdy"))
;; (presentation-typep "bye" '(one-of "hi" "hello" "howdy"))

(define-presentation-method type-for-subtype-tests ((type one-of))
  'string)



(define-presentation-type length (n))

(define-presentation-method presentation-typep (object (type length))
  (= (length object) n))

;; (presentation-typep "David" '(length 5))
;; (presentation-typep "David" '(length 9))

(define-presentation-method type-for-subtype-tests ((x length))
  'sequence)



(define-presentation-type currency (&optional (code '*) (low '*) (high '*))
  :inherit-from `(rational ,low ,high))

;; the code MUST be right but the ranges won't matter
(define-presentation-method type-for-subtype-tests ((x currency))
  `(currency ,code))


(define-presentation-method presentation-subtypep ((c currency) type)
  (with-presentation-type-decoded (name parameters)
      type
    (destructuring-bind (&optional (scode '*) (slow '*) (shigh '*))
        parameters
      (if (eq name 'currency)
          ;; check the code is the same
          (if (eq code scode)
              (subtypep `(rational ,low ,high)
                        `(rational ,slow ,shigh))
              ;; this means it is definitely NOT a subtype
              (values nil t))
          ;; otherwise we just check to see if my supertype is a subtype of type...
          (subtypep `(rational ,low ,high) type)))))

;; since currency objects are just represented as numbers all we can do is check range
;; and number type, not currency code. That's up to the above method
(define-presentation-method presentation-typep (object (type currency))
  (typep object `(rational ,low ,high)))


#+nil(climwi::funcall-presentation-generic-function
      presentation-subtypep '(currency :gbp)
      '(currency :gbp))

#+nil(climwi::funcall-presentation-generic-function
      presentation-subtypep '(currency :gbp 1 20)
      '(currency :gbp 1 10))

(define-presentation-method present ((object rational) (type currency) (stream stream) (view textual-view)
                                     &key acceptably)
  ;; we can still have currency symbols - they are acceptable
  (unless nil ; acceptably
    ;; !!! I need a better means of doing this sort of thing:-
    (cond ((eq code :usd)
           (format stream "$"))
          ((eq code :gbp)
           (format stream "~A" #\Pound_Sign))
          ((eq code :eur)
           (format stream "~A" (code-char 8364)))))
  (let* ((object (round-price-to-nearest-penny object))
         (pounds (truncate object))
         (pennies (* 100 (abs (- object pounds)))))
    (format stream "~A~:D.~2,'0:D"
            (if (< object 0) "-" "")
            (abs pounds) pennies))
  
  (unless acceptably
    (unless (member code '(:usd :gbp :eur))
      (format stream " ~A" (symbol-name code)))))

;; (present 12 'currency)
;; (present 12 '(currency :eur))
;; (present 12 '(currency :usd))
;; (present 12 '(currency :usd) :acceptably t)
;; (present 12 '(currency :pln))



(define-presentation-type percentage (&optional (low '*) (high '*))
  :options ((require-percent-sign t))
  :inherit-from `(rational ,low ,high))

;; (presentation-subtypep '(percentage 1 10) 'percentage)

(define-presentation-method presentation-typep (object (type percentage))
  (typep object `(rational ,low ,high)))

;; (presentation-typep 12 '(percentage 1 100))

(define-presentation-method present ((object rational) (type percentage) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably
  (format stream "~A%" (large-decimal-expansion object)))

;; (present-to-string 12 'percentage)
;; (present-to-string 1/3 'percentage)





;; this is going to be important for implementing web monad acceptor stuff...

(define-presentation-type typespec ()
  :options ((label "Typespec")))

(define-presentation-method presentation-typep (object (x typespec))
  (or (and (symbolp object)
           (not (keywordp object)))
      (and (listp object)
           (or (symbolp (first object))
               (and (listp (first object))
                    (symbolp (first (first object))))))))

;; (presentation-typep 'integer 'typespec)
;; (presentation-typep '(integer 1 10) 'typespec)

;; !!! I'm going to need some fairly extensive support for accepting these things too, since specifying
;; typespecs is one of the central things needed when setting up products.




(define-presentation-type named-type (name x))

(define-presentation-method presentation-typep (object (type named-type))
  (presentation-typep object x))

;; (presentation-typep 123 '(named-type foo number))
;; (presentation-typep 123 '(named-type foo string))

(define-presentation-method present ((object t) (type named-type) (stream t)
                                     (view t) &key acceptably)
  (present object x :stream stream :view view :acceptably acceptably))

;; Although it seems that I should have this I might not have to do so, because I might handle them in the accept for parameter-list
(define-presentation-method accept ((type named-type) (stream t) view
                                    &key error-if-not-eof
                                    delimiter-gestures 
                                    allow-command-invocation
                                    ;; name ; won't work -shadowed by the name parameter
                                    )
  (accept x :stream stream :view view :error-if-not-eof error-if-not-eof
          :delimiter-gestures delimiter-gestures
          :allow-command-invocation allow-command-invocation))

(define-presentation-method presentation-subtypep ((type named-type) putative-supertype)
  (presentation-subtypep x putative-supertype))

;; This suffers from the name shadowing problem
#+nil(define-presentation-method default-view-for-accepting ((type named-type) (stream t)
                                                        &key name)
  (default-view-for-accepting x stream :name name))

;; I should do other things too, like presentation-subtypep


;; and this is used for commands, including accepting from strings AND accepting froms of stuff
;; (we can use it w/o commands when we just want forms)


(define-presentation-type parameter-list (&rest ??parameters)
  :options ((separator " ")
            (separator-regex "^\\s+")
            (expected) ; if it's definitely expected to be there
            ;; set this option if you want to be able to accept incomplete parameter lists
            (incomplete-marker nil)
            ;; (name)
            )
  :inherit-from 'list)


;; this could be simplified a bit. Do I have something which does that?
(defun replace-typespec-named-definitions (spec definitions)
  (if (and definitions (listp spec))
      (with-presentation-type-decoded (name parameters options)
          spec
        (if (eq name 'named-type)
            ;; Just ignore the name bit
            (replace-typespec-named-definitions (second parameters)
                                                definitions)
            `((,name ,@ (mapcar (lambda (p)
                                  (or (and (symbolp p)
                                           (cdr (find p definitions :key #'car)))
                                      (and (presentation-typep p 'typespec)
                                           (replace-typespec-named-definitions p definitions))
                                      p))
                                parameters))
              ,@options)))
      spec))

(define-presentation-method presentation-typep (object (type parameter-list))
  ;; we have to go through each parameter checking for its presence
  ;; the only caveat is that if we want to have optional and key parameters things get more
  ;; complicated...
  ;; (it's probably quite easy to add in though)
  (and (listp object)
       (and (= (length object)
               (length ??parameters))
            (let ((names nil))
              (with-presentation-type-decoded (name parameters)
                  type
                name
                ;; I have to find any named ones...
                (loop for x in parameters
                   for value in object
                   when (and (consp x) (eq (first x) 'named-type))
                   do (push (cons (second x) value)
                            names))
                
                (every (lambda (object parameter)
                         (or (presentation-typep object parameter)
                             (and (listp object)
                                  (eq (first object)
                                      climwi::*incomplete-command-marker*))))
                       object
                       (mapcar (lambda (spec)
                                 (replace-typespec-named-definitions spec names))
                               parameters)))))))



(define-presentation-method presentation-subtypep ((type parameter-list) putative-supertype)
  (with-presentation-type-decoded (putative-name putative-parameters)
      putative-supertype
    (and (eq putative-name 'parameter-list) ; is this too specific?
         (= (length ??parameters)
            (length putative-parameters))
         (every #'presentation-subtypep
                ??parameters
                putative-parameters)
         (values t t))))



;; (presentation-typep (list 1 2 9) '(parameter-list integer integer (integer 1 15)))

;; a parameter list is now a very simple concept. No messy logic for getting labels or parameter names
;; or anything like that
;; What I *would* probably like to do is to put in key and rest parameters.
;; That, I think, would be useful

;; Now I need to accept parameter lists, describe typepsecs and so on. Complete things.

;; this is very VERY similar to (list ?x) except it has a fixed number of parameters each with
;; type specified
(define-presentation-method present ((object list) (type parameter-list) (stream stream)
                                     (view textual-view) &key acceptably)
  ;; I have to set something up so that we can present strings quoted...
  ;; !!! sensitivity should depend on whether we allow sensitive inferiors
  (loop for (x . r) on object
     for type in ??parameters
       when (and (listp x)
                 (eq (first x) climwi::*incomplete-command-marker*)
                 (not (typespec-option-p (second x) :default)))
     do (return-from present nil)
     do (if (and (listp x)
                 (eq (first x) climwi::*incomplete-command-marker*))
            (progn
              (write-sequence "#?" stream)
              (present (typespec-option (second x) :default) type :stream stream
                       :view view :acceptably t :sensitive t))
            (present x type :stream stream :view view :acceptably acceptably :sensitive t))
     when r do (format stream "~A" separator)))

;; FOR accept on this see 02.1

(define-presentation-method type-for-subtype-tests ((x parameter-list))
  (cons 'parameter-list
        (mapcar 'type-for-subtype-tests ??parameters)))

;; (type-for-subtype-tests '(parameter-list integer (scan "hi") (length 3)))



;; the following describes a list containing certain types
;; as well as being a subtype of itself parameter-list can be a subtype of it
;; also, list can be a subtype of it
;; unfortunately I have to implement that in ??? (TBC)
;; (define-presentation-type list-containing (?type))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Date and time

(define-presentation-type date-and-time (&optional (low '*) (high '*))
  :options ((format '("%d" "/" "%m" "/" "%Y" " - "
                      "%H" ":" "%M" ":" "%S")))
  :inherit-from `(integer ,low ,high))

(define-presentation-method presentation-typep (x (type date-and-time))
  (typep x `(integer ,low ,high)))

;; (presentation-typep (get-universal-time) 'date-and-time)

(define-presentation-method present ((object t) (type date-and-time) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably ; how do I make it (un)acceptable?
  (cybertiggyr-time:format-time stream format object))

;; now we can nicely do this:-
;; (present-to-string (get-universal-time) 'date-and-time)
;; cybertiggyr-time:*format-time-full*




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Date

;; !!! FIXME - make this accept db.date types as well, since those are better really
(define-presentation-type date (&optional (low '*) (high '*))
  :inherit-from `(integer ,low ,high)
  :options ((format '("%d" "/" "%m" "/" "%Y"))))

(define-presentation-method present ((object t) (type date) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably ; how do I make it (un)acceptable?
  (cybertiggyr-time:format-time stream format object))

(define-presentation-method present ((object t) (type date) (stream stream) (view html-value-view)
                                     &key acceptably)
  acceptably 
  (cybertiggyr-time:format-time stream '("%Y" "-" "%m" "-" "%d") object))

(define-presentation-method presentation-typep (x (type date))
  (typep x `(integer ,low ,high)))

;; (present-to-string (get-universal-time) 'date)



;; !!! If doing date and time arithmetic or anything it's worth using
;; !!! db.date instead of working with naked integers
;; I could, in fact, also make presentation-typep accept those as input

;; (presentation-typep (db.date (get-universal-time)) 'date)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Boolean

(define-presentation-type boolean ())

;; This is very restrictive. Is that desirable?
(define-presentation-method presentation-typep (x (type boolean))
  (or (eq x t)
      (eq x nil)))

;; (presentation-typep 12 'boolean)

(define-presentation-method present ((object (eql t)) (type boolean) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably
  (format stream "True"))

(define-presentation-method present ((object (eql nil)) (type boolean) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably
  (format stream "False"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General Sequence

;; This probably needs some options
(define-presentation-type sequence (&optional (element-type t)))

(define-presentation-method presentation-typep (x (type sequence))
  (or (presentation-typep x `(list ,element-type))
      (and (arrayp x)
           (error "Implement proper check for array"))))


;; (present (sql/from 'brand) 'sequence)
;; (present '(1 2 3) 'list)

;; It would be nice (perhaps) to implement the table view on text streams. I kind of have already

(define-presentation-method presentation-subtypep ((type sequence) putative-supertype)
  ;; are there any other cases we can determine this for?
  (with-presentation-type-decoded (putative-name putative-parameters)
      putative-supertype
    (and (eq putative-name 'sequence)   ; is this too specific?
         (presentation-subtypep element-type (first putative-parameters))
         (values t t))))



(define-presentation-type eql (object)
  )

(define-presentation-method presentation-typep (x (type eql))
  (eql x object))

;; (presentation-typep 'foo '(eql foo))

;; Should this shout? I don't really like it
(define-presentation-method present ((object symbol) (type eql) (stream stream) (view textual-view) &key acceptably)
  acceptably
  (write-sequence (string-capitalize (symbol-name object)) stream))

(define-presentation-method present ((object symbol) (type member) (stream stream) (view textual-view) &key acceptably)
  acceptably
  (write-sequence (string-capitalize (symbol-name object)) stream))



(define-presentation-type lambda (param &rest expression))

(define-presentation-method presentation-typep (x (type lambda))
  (funcall (eval `(lambda ,param ,@expression)) x))


(define-presentation-type has (accessor &optional (?type '(not (eql nil))))
  )

(define-presentation-method presentation-typep (x (type has))
  (presentation-typep (funcall accessor x)
                      ?type))

;; (presentation-typep "hello" '(and string (has length (integer 3 10))))
;; (presentation-typep "h" '(and string (has length (integer 3 10))))

;; (note: the 'accessor' can be any funcallable object including lambdas)
;; also, symbol lists are auto composed by s/funcall which allows us to chain things




;; This is copy pasted from src/util.lisp, but I think it's a useful thing to have
(defparameter *postcode-regex* "[A-Z]{1,2}[0-9R][0-9A-Z]? [0-9][ABD-HJLNP-UW-Z]{2}"
  "Regular expression to match postcodes")

(define-presentation-type uk-postcode ()
  :inherit-from 'string)

(define-presentation-method presentation-typep (x (type uk-postcode))
  (and (stringp x)
       (cl-ppcre:scan *postcode-regex* x)))

;; (presentation-typep "TQ12 4NR" 'uk-postcode)
;; (presentation-typep "TQ124NR" 'uk-postcode)

(define-presentation-method present ((object string) (type uk-postcode) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably ; how do I make it (un)acceptable?
  (write-sequence object stream))


(define-presentation-type description ())


(define-presentation-method presentation-typep (x (type description))
  (stringp x))

(define-presentation-method present ((object string) (type description) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably ; how do I make it (un)acceptable?
  (format stream "~S" object))


(define-presentation-type numeric-range ())

(define-presentation-method presentation-typep (x (type numeric-range))
  (or (and (presentation-typep x '(parameter-list (eql integer) integer integer))
           (< (first (cdr x)) (second (cdr x))))
      (presentation-typep x '(parameter-list (eql integer) integer))))

(define-presentation-method present ((object list) (type numeric-range) (stream stream) (view textual-view)
                                                   &key acceptably)
  acceptably
  ;; Is there an easier way to handle these? 
  (let ((object (cdr object)))
    (format stream "~A-~A" (first object) (second object))))

;; (present-to-string '(integer 1 100) 'numeric-range)
