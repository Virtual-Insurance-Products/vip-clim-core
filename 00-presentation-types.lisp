


(in-package :clim-internals)

(defparameter *incomplete-command-marker* (gensym "INCOMPLETE"))



;; In cases where we would like to know which component of some typespec failed (presuming we expected the type check to succeed) it can be useful to be able to catch a more specific reason as to why the type failed
;; ergo I will define this condition to allow such information to be aquired
;; SEE ALSO: presentation-typep for or, which effectively has to defeat this measure

;; now there are 2 things we need to deal with:-
;; 1. the or type. We can't catch signals from failure of the non terminal bit
;; 2. the cunning optimisation for checking list elements &c which means ?element-type is a function, not a typespec
(define-condition presentation-type-failure ()
  ((object :initarg :object)
   (expected-type :initarg :expected-type)
   (context :initarg :context)))


;; SO. We have:-
;; 1. presentation typespecs
;; 2. presentation types (which will be instances of clos classes)

;; 1 will look very much like standard CL type specifications and will be a superset of them

;; Given the above we will be able to:-
;; 1. determine if an object satisifies some type with presentation-typep
;; 2. ascertain some kind of default presentation typespec for an object
;; 3. accept, from various source, instances of the type
;; 4. present, to various places and in various ways, instances of the type



;; This is somewhat different from the standard definition
;; I was trying to optimise things like (presentation-typep x '(list string))
;; It's probably not worth doing so though. 
(defun presentation-typep (object type)
  (funcall (presentation-type-checker type) object))


;; FURTHER NOTE: having the same cunning checker expansion with ? prefixed parameter
;; (which will be handled by define-presentation-method) is still going to be useful
;; because it will give me a way to fully expand the type checker functions from a typespec
;; before iterating the subtype checker over (eg) lists and also make the command
;; table evaluations more efficient.

;; we won't lose noticeable runtime efficiency with this new system.


;; now, we have to keep a registry of these types somewhere so we know what options and parameters
;; to destructure from the lists for the type specs and/or created instances...


;; typespec can take the following forms:-
;; (%typespec-name 'integer)
;; (%typespec-name '(integer 1 10))
;; (%typespec-name '((integer 1 10) :prompt "Enter a smallish number"))

;; This could also be implemented in terms of with-presentation-type-decoded
(defun presentation-type-name (type)
  (%typespec-name type))

(defun %typespec-parameters (spec)
  (when (consp spec)
    (if (consp (first spec))
        (cdr (first spec))
        (cdr spec))))

;; (%typespec-parameters 'integer)
;; (%typespec-parameters '(integer 1 10))
;; (%typespec-parameters '((integer 1 10) :name "Boris"))


;; !!! Quite a few missing as per http://bauhh.dyndns.org:8000/clim-spec/23-3.html#_1173

(defun typespec-option (spec name)
  "Returns the value of the option for the given typespec.
Also returns whether the option exitsts or not, to differentiate between 
missing options and nil options."
  (let ((option (member name (%typespec-options spec))))
    (values 
     (second option)
     (and option t))))


(defun typespec-option-p (spec name)
  "Returns true if the given option has been specified for the typespec."
  (multiple-value-bind (val exists?)
      (typespec-option spec name)
    (declare (ignore val))
    exists?))






;; this will fail to make, for example, a prototype integer or string
;; (was prototype-or-error)
(defun prototype-or-base (type-name)
  (let ((type-class (find-class (%typespec-presentation-class-name type-name) nil)))
    (if type-class
        (make-instance type-class)
        (let ((class (find-class type-name nil)))
          (if (and class (typep class 'standard-class))
              (ccl:class-prototype class) ; this is a better way to get a prototype
              ;; rather than erroring I will make an instance of the base
              ;; since it provides default methods for type checking by using the typep
              ;; and for doing other things
              ;; BUT it should be noted that this doesn't g... (seems I didn't finish that sentence!)
              (make-instance 'thing-type))))))




;; NOTE - this is a significant deviation from standard CLIM implementation
;; Basically, the result of (funcall-presentation-generic-function presentation-typep <type>) is
;; NOT to check the type of the object (which isn't even passed) but, rather, it returns a type checking function.

;; The idea of that is to bypass the very dynamic presentation-g-f dispatch system which would
;; otherwise have to be used when type checking against, for example, '(list string)
;; - in that way the presentation-typep method for 'list can get a function to check the element type
;; and then apply that to every element looking for mismatch without having to go through
;; generic dispatch.

;; However, by the time we get to defining actual presentation-typep
;; methods, things look normal again (see below).
(defun presentation-type-checker (type)
  "Return a function which checks for membership of some type given some representation of the type"
  (etypecase type
    (function type)
    ;; (presentation-type (%presentation-type-checker type))
    ;; a typespec. Transform this into the type instance
    (t
     (funcall-presentation-generic-function presentation-typep
                                            type)
     ;; (dependent-type-checker (presentation-type-from-type-spec type))
     )))


;; This is used in determining whether some presented object satisfies some subtype test
(defun type-for-subtype-tests (type)
  (funcall-presentation-generic-function type-for-subtype-tests type))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERLUDE

;; !!! I've had to move this up here
(defclass textual-view ()
  ())


;; HTML values may need formatting differently to their display
(defclass html-value-view (textual-view) 
  ())



;; define some types...

(defmethod presentation-type-of ((x t)) (type-of x))
(defmethod presentation-type-of ((x integer)) 'integer)
(defmethod presentation-type-of ((x string)) 'string)
(defmethod presentation-type-of ((x number)) 'number)
(defmethod presentation-type-of ((x ratio)) 'ratio)
(defmethod presentation-type-of ((x float)) 'float)
(defmethod presentation-type-of ((x standard-object))
  (class-name (class-of x)))

;; (presentation-type-of (vip::db 'vip::db-quote 123))
;; (presentation-type-of 12)

;; This is done so that we can define default type checkers for all things as follows...
;; !!! I have called this thing, and it is the superclass of all other presentation-type classes
;; I called it thing so as not to get confused with anything else. That might be wrong though
;; !!! This needs options, which will be standard options
(define-presentation-type thing (&rest parameters)
  :options (label))


;; so this will catch all the basic things like string, number and so on
(define-presentation-method presentation-typep (object (type thing))
  ;; this intentionally ignores the options, which should have no effect on type membership
  (with-presentation-type-decoded (name params)
      type
    (if params
        (typep object (cons name params))
        (typep object name))))

;; type checker for clos instances w/o a specific type defined here...
(define-presentation-method presentation-typep (object (type standard-object))
  ;; only the name can be given in this case
  (with-presentation-type-decoded (name)
      type
    (typep object name)))


;; We need to allow a type to supply a different type when subtype tests are required
(define-presentation-method type-for-subtype-tests ((type thing)) type)
(define-presentation-method type-for-subtype-tests ((type standard-object)) type)


;; this will work where the typespec of the presentation type is ALSO a normal CL typespec
;; (which will often be the case)
(define-presentation-method presentation-subtypep ((type thing) super)
  (subtypep type super))


;; the only reason I would want to declare this here is to be able to inherit from it
;; BUT I don't think I have to declare it in order to do that.
;; Basically, all the base CL type things can just be done via thing-type
;; I think. (unless I find a problem with that)

(define-presentation-type number (&optional (low '*) (high '*)))
(define-presentation-type real (&optional (low '*) (high '*))
  :inherit-from `(number ,high ,low))
(define-presentation-type float (&optional (low '*) (high '*))
  :inherit-from `(real ,high ,low))
(define-presentation-type rational (&optional (low '*) (high '*))
  :inherit-from `(number ,high ,low))
(define-presentation-type integer (&optional (low '*) (high '*))
  :inherit-from `(rational ,high ,low))
(define-presentation-type ratio (&optional (low '*) (high '*))
  :inherit-from `(rational ,high ,low))

(define-presentation-type string ())
(define-presentation-type function ())

;; (presentation-typep "hi" 'string)

;; actually that's wrong. I'm going to have to at least declare all those types because I will want to specialise
;; accept and present on them. Good point.

;; When we want to check a presented number to see if it's a subtype of a type
;; with range parameters it's really meaningless to consider the range parameters
(define-presentation-method type-for-subtype-tests ((type number))
  (with-presentation-type-decoded (name)
      type
    name))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-presentation-type and (&rest ??c))

(define-presentation-method presentation-typep (object (x and))
  (every (lambda (x)
           (presentation-typep object x))
         ??c))

;; (presentation-typep 13 '(and number (satisfies oddp)))
;; (presentation-typep 12 '(and number (satisfies oddp)))

(define-presentation-method type-for-subtype-tests ((x and))
  ;; The use of the and is to specify the type, followed by further type constraints
  ;; It isn't, therefore, useful to consider anything other than the first ??c for
  ;; type-for-subtype-tests
  (type-for-subtype-tests (first ??c)))


(define-presentation-method present ((object t) (type and) (stream t)
                                     (view t) &key acceptably)
  acceptably 

  (present object (first ??c) :stream stream :view view :acceptably acceptably :sensitive nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-presentation-type or (&rest ??c))

(define-presentation-method presentation-typep (object (x or))
  (some (lambda (x)
          (presentation-typep object x))
        ??c))

(define-presentation-method type-for-subtype-tests ((x or))
  `(or ,@ (mapcar #'type-for-subtype-tests ??c)))


(define-presentation-method present ((object t) (type or) (stream t)
                                     (view t) &key acceptably)
  acceptably 

  (if object
      (if (presentation-typep object (first ??c))
          (present object (first ??c) :stream stream :view view :acceptably acceptably :sensitive nil)
          (present object (cons 'or (cdr ??c))
                   :stream stream :view view :acceptably acceptably :sensitive nil))
      (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-presentation-type not (?type))

(define-presentation-method presentation-typep (object (x not))
  (not (presentation-typep object ?type)))

(define-presentation-method type-for-subtype-tests ((x not)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-presentation-type list (&optional (?element-type t))
  :options ((label "List"
                   ;; Can I get the following to work?
                   ;; (format nil "List of ~A" (describe ?element-type))
                   )
            (separator ", ")
            (separator-regex "^\\s*,\\s*"))
  ;; :inherit-from 'sequence
  )

;; (presentation-typep '(1 2 3) 'list)

(define-presentation-method presentation-typep (object (x list))
  (and (listp object)
       (or (eq ?element-type t)
           (every (presentation-type-checker ?element-type) object))))

;; (presentation-typep '(1 2 3) '(list number))
;; (presentation-typep '(1 2 3) '(list (and number (satisfies oddp))))
;; (presentation-typep '(1 5 3) '(list (and number (satisfies oddp))))

(define-presentation-method presentation-subtypep ((type list) putative-supertype)
  (with-presentation-type-decoded (putative-name putative-parameters)
      putative-supertype
    (and (eq putative-name 'list)
         (presentation-subtypep ?element-type (first putative-parameters))
         (values t t))))

;; This is an interesting case
;; It depends on whether we only accept things DECLARED as lists of a type
;; or as things which HAPPEN TO BE lists of that type too
;; The latter seems more consistent with the rest...
(define-presentation-method type-for-subtype-tests ((x list))
  'list)

(define-presentation-method present ((object list) (type list) (stream stream) (view textual-view)
                                     &key acceptably)
  (when object
    (loop for (a . r) on object
       do
         (present a ?element-type :stream stream :view view :acceptably acceptably)
       when r
         do (format stream "~A" separator)
         )))


(define-presentation-type member (&rest objects))
(define-presentation-method presentation-typep (object (x member))
  (typep object (cons 'member objects)))




(defun %type-supertype (type)
  (let ((def (gethash (%typespec-name type) *type-definitions*)))
    (if def
        ;; then we can just make the supertype...
        (funcall (third def) type)
        ;; otherwise maybe it names a standard class
        (with-presentation-type-decoded (name)
            type
          (let ((class (find-class name)))
            (if class
                (let ((supers (ccl:class-direct-superclasses class)))
                  (if (cdr supers)
                      (cons 'and (mapcar #'class-name supers))
                      (class-name (first supers))))))))))


;; this here needs to just get the supertypes by calling the function which was defined to
;; specify the supertype (which defaults to t)
(defun map-over-presentation-type-supertypes (function type)
  (let ((super (%type-supertype type)))
    (when super
      (funcall function (%typespec-name super) super)
      (map-over-presentation-type-supertypes function super))))

;; presentation-subtypep

(defun presentation-subtypep (type putative-supertype)
  "Determine whether TYPE is a subtype of PUTATIVE-SUPERTYPE.
Returns two values: indicating whether it is and whether we're sure"

  (with-presentation-type-decoded (type-name type-parameters)
      type
    (with-presentation-type-decoded (supertype-name supertype-parameters)
        putative-supertype
      (cond ((eq type-name supertype-name)
             (cond ((null supertype-parameters)
                    (values t t))
                   ((equal supertype-parameters type-parameters)
                    (values t t))
                   (t (multiple-value-bind (subtype-p known-p)
                          (funcall-presentation-generic-function presentation-subtypep type putative-supertype)
                        (when (and subtype-p (not known-p))
                          (error "The call to ~S on type ~S only returned a single T but should return 2" 'presentation-subtypep type))
                        (values subtype-p known-p)))))
          
            ((eq type-name 't)    (values nil t))
            ((eq type-name 'nil)  (values t t))
            ((eq supertype-name 't)   (values t t))
            ((eq supertype-name 'nil) (values nil t))

            ((eq type-name 'and)
             ;; Yes, we really call SOME, not EVERY.  For example,
             ;; (AND INTEGER (SATISFIES ODDP)) is a subtype of INTEGER
             (if (some #'(lambda (type)
                           (with-presentation-type-decoded (type-name) type
                             (unless (member type-name '(not satisfies))
                               (presentation-subtypep type putative-supertype))))
                       type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq type-name 'or)
             (if (every #'(lambda (type)
                            (presentation-subtypep type putative-supertype))
                        type-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'and)
             (if (every #'(lambda (supertype)
                            (with-presentation-type-decoded (type-name) supertype
                              (unless (member type-name '(not satisfies))
                                (presentation-subtypep type supertype))))
                        supertype-parameters)
                 (values t t)
                 (values nil nil)))
            ((eq supertype-name 'or)
             (if (some #'(lambda (supertype) (presentation-subtypep type supertype))
                       supertype-parameters)
                 (values t t)
                 (values nil nil)))
            
            ;; handle and and or in super and type
            ;; (couldn't that be done in a presentation method? Not sure

            ;; then use type inheritance
            (t
             (let ((ntype (%type-supertype type)))
               (if ntype
                   (presentation-subtypep ntype putative-supertype)
                   ;; this means we've hit the top and not found a match
                   ;; However, this should only ever happen by hitting T I think
                   (values nil t))))))))






;; output...

(defmethod stream-default-view ((stream stream))
  (make-instance 'textual-view))

(defmethod stream-default-view ((stream t)) nil)



(defun default-view-for-object (object &optional (type (presentation-type-of object)) stream)
  (or (awhen (typespec-option type :view)
        (if (and (listp it)
                 (symbolp (first it)))
            (apply #'make-instance it)
            it))
      (stream-default-view stream)
      (funcall-presentation-generic-function default-view-for-object object type stream)))


;; (default-view-for-object 12)
;; (default-view-for-object 12 'integer)

(define-presentation-method default-view-for-object ((x t) (type thing) (stream t))
  (make-instance 'textual-view))


;; This is probably supposed to set up some dynamic bindings
;; !!! It would be good in some cases if the presentation-typep failure in here (which is going to error anyway) could error further in when checking
;; so, for example, when type checking a (sequence <T>) we could error on the specific element rather than on the whole object
;; that would give more localised error reporting. I'm sure this could somehow be done with conditions
(defun present (object &optional (type (presentation-type-of object))
		&key
                  (stream *standard-output*)
                  ;; I think I will change this from the standard...
                  (view (default-view-for-object object type stream))
                  ;; (view (stream-default-view stream))
                  ;; modifier
                  acceptably
                  ;; (for-context-type type)
                  ;; single-box
                  ;; (allow-sensitive-inferiors t)
                  (sensitive t)
                  ;; (record-type 'standard-presentation)
                  )

  ;; !!! Presenting nil insensitively is a sneaky way of implementing accept (as used for web-monad)
  (unless (or (and (not object)
                   (not sensitive))
              ;; I can catch these signals to make a more specific error message
              (handler-case
                  (presentation-typep object type)
                (presentation-type-failure (f)
                  (error "Object ~A is not of the expected type ~A in presentation-typep within ~A"
                         (slot-value f 'object)
                         (slot-value f 'expected-type)
                         (slot-value f 'context)))))
    
    (error "~A is not of type ~A in present"
           (let ((x (format nil "~S" object)))
             (if (> (length x) 255)
                 (format nil "~A..." (subseq x 0 255))
                 x))
           type))

  ;; !!! type should be expanded to give real type as (expand-presentation-type-abbreviation type)
  (stream-present stream object type
                  :view view
                  :sensitive sensitive
                  :acceptably acceptably
                  ))

(defun present-to-string (object &optional (type (presentation-type-of object))
                                   &key view
                                     (acceptably nil))
  (with-output-to-string (stream)
    (unless view
      (setf view (or (stream-default-view stream)
                     (default-view-for-object object type))))
    (present object type 
             :stream stream 
             :view view 
             :acceptably acceptably)))





;; (funcall-presentation-generic-function accept 'integer stream view)

(defmethod stream-present ((stream stream) object type &key view acceptably sensitive)
  (declare (ignore sensitive))
  ;; since we have no way to make real presentations on a normal stream the following will do...
  (funcall-presentation-generic-function
   present object type stream view :acceptably acceptably))


;; !!! I think this one isn't needed. The below one will be fine
(define-presentation-method present ((object t) (type thing) (stream stream) (view textual-view) &key acceptably)
  (if acceptably
      (format stream "~S" object)
      (format stream "~A" object)))


;; This will work for absolutely anything I think (including CLOS intances)
(define-presentation-method present ((object t) (type t) (stream stream) (view textual-view) &key acceptably)
  type
  (if acceptably
      (format stream "~S" object)
      (format stream "~A" object)))




;; now things like this work:-
;; (present 123)
;; (present "hi" 'string :acceptably t)



;; so, every type allows a 'name' option, which is used for parameter list types and gets spliced in
;; let's also allow 
