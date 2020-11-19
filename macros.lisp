
(in-package :clim-internals)


(defmacro with-presentation-type-decoded ((name &optional parameters options) type &body body)
  (let ((typevar (gensym "TYPE")))
    `(let ((,typevar ,type))
       (let ((,name (%typespec-name ,typevar))
             ,@ (when parameters
                  `((,parameters (%typespec-parameters ,typevar))
                    ,@ (when options
                         `((,options (%typespec-options ,typevar)))))))
         ,@body))))


;; !!! To work *properly* this would need to do eval-when compiling, since that will be essential
;; At the moment I'm not ever compiling things - just loading and then dumping an image, so I can sidestep
;; those issues. I should fix it if I ever want to use ASDF though
(defmacro define-presentation-type (type-name parameters &key options
                                                           ;; inherit from the t-type class by default
                                                           ;; (unless we ARE the t-type class)
                                                           (inherit-from
                                                            t
                                                            #+nil(unless (eq type-name t)
                                                                   't-type)))
  (let ((option-lambda-list `(&rest other-options &key ,@options &allow-other-keys)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',type-name *type-definitions*)
             (list ',parameters
                   ;; !!! This is necessary because if something is handled by a superclass
                   ;; the superclass won't know what options to allow
                   ',option-lambda-list
                   ;; ',(cons '&key options)
                   #'(lambda (spec)
                       ;; !!! I have to call the expander here directly because I haven't
                       ;; yet defined this thing into place
                       ,(expand-with-presentation-type-bits
                         parameters
                         '%typespec-parameters
                         'spec
                         (list (expand-with-presentation-type-bits
                                option-lambda-list
                                ;; (cons '&key options)
                                '%typespec-options 'spec
                                (list inherit-from))))
                     
                       #+nil(with-presentation-type-options (,type-name spec)
                              (with-presentation-type-parameters (,type-name spec)
                                ,inherit-from)))))
       (defclass ,(%typespec-presentation-class-name type-name)
           ,(if (eq type-name 'thing)
                nil
                (if (eq inherit-from t)
                    '(thing-type)
                    ;; otherwise we have to figure
                    ;; out what the superclass should be
                    ;; The only robust(ish) way I can think of to figure this out is to use eval
                    ;; https://www.youtube.com/watch?v=WTG2amHSkXI
                    (let* ((spec (eval (expand-with-presentation-type-bits
                                        parameters
                                        'quote
                                        (mapcar (constantly t)
                                                (loop for x in parameters
                                                   while (not (member x standard-lambda-list-keywords))
                                                   collect t))
                                        (list inherit-from))))
                           (name (%typespec-name spec)))
                      (mapcar (lambda (type)
                                (if (find-class (%typespec-presentation-class-name type))
                                    (%typespec-presentation-class-name type)
                                    type))
                              (cond ((eq name 'and)
                                     ;; make it inherit from all the things...
                                     (mapcar '%typespec-name (cdr spec)))
                                    ((eq name 'or)
                                     (error "Don't know what to do with ~A as a supertype" spec))
                                    (t (list name)))))))
       
         ()))))


;; (define-presentation-type integer (&optional low high) :options ((base 10)))


(defmacro with-presentation-type-parameters ((type-name type) &body body)
  (expand-with-presentation-type-bits (type-definition-parameters type-name)
                                      '%typespec-parameters type body))

;; (with-presentation-type-parameters (integer '(integer 1 10)) (list min max))
;; (with-presentation-type-parameters (integer 'integer) (list min max))


(defmacro with-presentation-type-options ((type-name type) &body body)
  (expand-with-presentation-type-bits (type-definition-options type-name)
                                      '%typespec-options type body))

;; This doesn't generate an expansion for options at the moment since nothing has any. I need to fix that...
;; (with-presentation-type-options (integer '((integer 1 10) :label "Price")) 12)

(defun get-qualifier-from-args (args)
  "If the arguments to define-presentation-method starts with a keyword this is the qualifier. "
  (when (keywordp (first args))
      (first args)))

(defun get-parameters-from-args (args)
  "If the arguments to define-presentation-method starts with a keyword this is the qualifier,
ignore this and return the second member. "
  (if (keywordp (first args))
      (second args)
      (first args)))

(defun get-body-from-args (args)
  "If the arguments to define-presentation-method starts with a keyword this is the qualifier,
ignore this and return the third member. "
  (if (keywordp (first args))
      (cddr args)
      (cdr args)))

(defmacro define-presentation-method (name &rest args)
  "
 Define one of the following presentation methods:-

 (define-presentation-method presentation-typep (object type) ...)
 (define-presentation-method presentation-subtypep (type putative-supertype) ...)
 (define-presentation-method present (object type stream view &key acceptably) ...)
 (define-presentation-method default-view-for-object (object type stream) ...)

 (define-presentation-method accept (type stream view &key error-if-not-eof) ...)
 (define-presentation-method default-view-for-accepting (type stream &key name) ...)

 (define-presentation-method type-for-subtype-tests (type) ...)
"
  ;; do things with the parameters
  ;; the stream parameter (where applicable) can be a naked keyword in which case we will eql it
  ;; the type parameter varies depending on which method we are defining
  (let ((qualifier (get-qualifier-from-args args))
        (parameters (get-parameters-from-args args))
        (body (get-body-from-args args))
        (type-key-arg nil))
    (let* ((method-parameters (loop for p in parameters
                                 for i from 0
                                 for type-param-p = (= i (type-arg-position name))
                                 when type-param-p
                                 do (setf type-key-arg
                                          (list 'clim-internals::type-key
                                                ;; !!! But this is supposed to be quite a different symbol...
                                                ;; (it doesn't matter because the things which calls %accept takes care of it)
                                            
                                                ;; (intern (format nil "(presentation-type "))

                                                ;; BUT, surely we want this specialised on the type class?
                                                (if (find-class (%typespec-presentation-class-name (second p))
                                                                nil)
                                                    (%typespec-presentation-class-name (second p))
                                                    ;; otherwise we have 2 choices:-
                                                    ;; either it names a clos class or it names a type which isn't defined
                                                    ;; in the latter case we really aren't going to be able to win
                                                    (second p)
                                                    )
                                                ))
                                 collect (if type-param-p
                                             (if (consp p)
                                                 (first p)
                                                 (error "Type parameter must be specialised"))
                                             p)))
           (type-arg (nth (type-arg-position name) method-parameters))
           (type-name (second (nth (type-arg-position name) parameters))))
    
      ;; now, I'm going to do a little bit of jiggery pokery when defining
      ;; type validation methods, because I want a method which can return (efficiently) a validation function for type
      ;; members
      `(defmethod ,(presentation-method-internal-name name) 
           ,@(when qualifier 
                 (list qualifier))
         ,(cons type-key-arg
                ;; !!! A little sleight of hand
                (if (eq name 'presentation-typep)
                    (cdr method-parameters)
                    method-parameters))
         (declare (ignorable ,type-arg))
         (with-presentation-type-parameters (,type-name ,type-arg)
           ;; now a minor optimization
           ;; If the presentation type parameters are themselves types, signified by a leading ?
           ;; then I will get resolve them to type checkers here IFF we are in the p-typep method
           ;; !!! EXTEND FOR LISTS OF TYPES TOO via ??
           ;; (see and for example)
           ;; (not too important)
           ,@ (when (eq name 'presentation-typep)
                (loop for p in (get-all-params (type-definition-parameters type-name))
                   when (eql #\? (elt (symbol-name p) 0))
                   collect (if (eql #\? (elt (symbol-name p) 1))
                               `(setf ,p (mapcar #'presentation-type-checker ,p))
                               `(setf ,p (presentation-type-checker ,p)))))
              (with-presentation-type-options (,type-name ,type-arg)
                ,(if (eq name 'presentation-typep)
                     `(lambda (,(first method-parameters))
                        (flet ((call-next-method ()
                                 (funcall (call-next-method) ,(first method-parameters))))
                          (block ,name
                            ,@body)))
                     `(block ,name
                        ,@body))))))))

;; (define-presentation-method accept ((type and) stream))
;; (define-presentation-method present (object (type date) stream view) (+ 1 2))

;; (accept and present are interned in the cl-user package. That, perhaps, means that we should do the same here)


(defmacro funcall-presentation-generic-function (name &rest args)
  ;; !!! I haven't made the presentation method hash table here
  (when (eq name 'presentation-typep)
    (setf args (cons 'object args)))
  (let* ((rebound-args (loop for arg in args
                          unless (symbolp arg)
                          collect (list (gensym "ARG") arg)))
         (gf-name (presentation-method-internal-name name))
         (type-spec-var (nth (type-arg-position name) args))
         (call `(,gf-name (prototype-or-base (presentation-type-name
                                              ,(or (first (find type-spec-var rebound-args :key #'second))
                                                   type-spec-var)))
                          ,@(mapcar #'(lambda (arg)
                                        (or (first (find arg rebound-args :key #'second))
                                            arg))
                                    ;; hack to make presentation-typep actually return a function of object
                                    ;; this will make repeated evaluation of presentation-type on the same type
                                    ;; be quicker
                                    ;; it gives us the ability to have a first class checker
                                    (if (eq name 'presentation-typep)
                                        (cdr args)
                                        args)))))
    (if rebound-args
        `(let ,rebound-args
           ,call)
        call)))




(defmacro with-output-as-presentation ((stream object type
                                               &key
                                               (element-type nil)
                                               (event-handlers))
                                       &body content)

  `(%do-output-as-presentation ,stream ,object ,type
                               (lambda ()
                                 ,@content)
                               :element-type ,element-type
                               ,@ (when event-handlers
                                    (list :event-handlers event-handlers))))
