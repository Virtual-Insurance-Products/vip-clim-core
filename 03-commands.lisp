
(in-package :clim-internals)

;; QUESTION: Should I change this to conform more closely to the CLIM specification?

;; definition of commands
;; to start with we must define some things needed...


;; this is for a command entered
;; !!! This will be rather unusual in having a parameter which is a complex CLOS object
;; !!! Also, maybe it should have parameters for the parameter types and return types
;; that way I can (accept '(command active-command-table parameter-list integer) to accept only
;; integer returning commands
;; I guess that would simplify my definition of the command handling bit in accept quite a lot
;; Probably a Good Idea
(define-presentation-type command
    (&key (table 'active-command-table)
          (parameters 'parameter-list)
          (return-type t)))


(define-presentation-type command-with-unevaluated-parameters
    (&key (table 'active-command-table)
          (parameters 'parameter-list)
          (return-type t)))


;; now, to do things with the unevaluated commands we need to transform the parameter list like so...
(defun unevaluated-parameter-list-type (parameter-list)
  (with-presentation-type-decoded (name parameters options)
      parameter-list
    `((,name ,@(mapcar (lambda (parameter)
                         (with-presentation-type-decoded (name parameters)
                             parameter
                           `(or (command-with-unevaluated-parameters :return-type (,name ,@parameters))
                                ,parameter)))
                       parameters))
      ,@options)))





;; perhaps there should be somewhere where we can find a list of globally defined command tables?
(defun resolve-command-table (command-type)
  
  ;; !!! This WILL NOT WORK
  ;; (but something like it will - need to get the default value)
  (with-presentation-type-decoded (name parameters)
      command-type
    (declare (ignore name))
    (destructuring-bind (&key (table 'active-command-table) parameters return-type)
        parameters
      (declare (ignore parameters return-type))
      (if (symbolp table)
          (funcall table)
          table))))


;; !!! Look at the specified command table
(define-presentation-method presentation-typep (object (type command))
  (and (listp object)
       (symbolp (first object))
       ;; !!! Should I check that it is found?
       (let ((command (find-command (first object)
                                    (resolve-command-table type))))
         (and command
              (presentation-subtypep (command-parameters-type command)
                                     parameters)
              (presentation-subtypep (command-return-type command)
                                     return-type)))))



;; !!! Also check the parameter types
(define-presentation-method presentation-typep (object (type command-with-unevaluated-parameters))
  (and (listp object)
       (symbolp (first object))
       (let ((command (find-command (first object)
                                    (resolve-command-table type))))
         (and command
              (presentation-subtypep (command-parameters-type command)
                                     parameters)
              (presentation-subtypep (command-return-type command)
                                     return-type)))))


;; !!! IMPLEMENT THESE PROPERLY
;; (although most of the time if we get here the answer is going to be no)
(define-presentation-method presentation-subtypep ((type command) putative-supertype)
  putative-supertype
  (values nil nil))
(define-presentation-method presentation-subtypep ((type command-with-unevaluated-parameters) putative-supertype)
  putative-supertype
  (values nil nil))



;; next let's define the objects we are going to use to keep track of these things...


;; I'm going to start with a very simple minded definition
(defclass command-table ()
  ((name :initarg :name :reader command-table-name)
   (inherit-from :initarg :inherit-from :initform '()
                 :reader command-table-inherit-from)
   ;; I'll just store this as a list
   ;; we will want to find things in various ways
   (commands :initform nil :accessor commands)
   (presentation-translators :initform (make-hash-table)
                             :reader presentation-translators)))



(defun apply-with-command-table-inheritance (fun command-table)
  (funcall fun command-table)
  (mapc #'(lambda (inherited-command-table)
	    (apply-with-command-table-inheritance
             ;; This is slightly different from McCLIM which has 'find-command-table'
             ;; what does that do?
             ;; I see - it implements a *command-tables* thingy for storing them globally
             ;; I don't know that I need that really. Let's leave it like this for now...
             ;; (you can also just reference them directly in McCLIM too)
	     fun inherited-command-table))
	(command-table-inherit-from command-table)))


(defmethod print-object ((table command-table) stream)
  (print-unreadable-object (table stream)
    (apply-with-command-table-inheritance
     (lambda (c)
       (format stream "~%~%Command table: ~A~%=========~%~%" (command-table-name c))
       (loop for entry in (commands c)
          do (format stream "~A~%" entry)))
                                          table)))







(defparameter *active-command-table* nil)

(defun active-command-table ()
  *active-command-table*)




(defclass command-table-entry ()
  ((action :initarg :action :reader command-action)
   (parameters :initarg :parameters :reader command-parameters)
   ;; I'm going to leave this very general
   ;; it will just be an ?alist
   (symbol-name :initarg :symbol-name :reader command-symbol-name)
   (name :initarg :name :reader command-name)
   (options :initarg :options :reader command-options :initform nil)
   ;; nil shall mean that we don't know. It is the empty type
   ;; WRONG - t shall mean 'an object of some type' but we can't be more specific
   (return-type :initarg :return-type :reader command-return-type :initform t)
   ))

(defmethod command-option ((command command-table-entry) name)
  (second (member name (command-options command))))

(defmethod command-option ((command list) name)
  (command-option (find-command command (active-command-table)) name))


;; !!! This could be slightly improved...
(defmethod print-object ((entry command-table-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "~S (~A) :: ~A -> ~A"
            (command-name entry)
            (command-symbol-name entry)
            (command-parameters entry)
            (command-return-type entry))))

(defun make-parameter-list (parameters)
  (cons 'parameter-list
        (loop for (name type) in parameters
           for pname = (%typespec-name type)
           for parameters = (%typespec-parameters type)
           for options = (append (%typespec-options type)
                                 `(:name ,name))
           collect `((,pname ,@parameters) ,@options))))

;; !!! I may change the implementation to store the parameter list in the first place
(defmethod command-parameters-type ((x command-table-entry) &rest options)
  (cons (make-parameter-list (command-parameters x))
        options))


;; Now, for the first time, I'm going to define a presentation type for the above clos class...
(define-presentation-type command-table-entry (&optional (parameter-list 'parameter-list)
                                                         (return t))
  )

(define-presentation-method presentation-typep (object (entry command-table-entry))
  (and (typep object 'command-table-entry)
       (presentation-subtypep (command-parameters-type object)
                              parameter-list)
       (presentation-subtypep (command-return-type object)
                              return)))


;; the main reason for wanting to define that is so that I can implement this...
(define-presentation-method presentation-subtypep ((c command-table-entry) putative-supertype)
  (with-presentation-type-decoded (putative-name putative-parameters)
      putative-supertype
    (and (eq putative-name 'command-table-entry)
         (or (not putative-parameters)
             (destructuring-bind (&optional (putative-plist 'parameter-list)
                                            (putative-return t))
                 putative-parameters
               (and (presentation-subtypep parameter-list putative-plist)
                    (presentation-subtypep return putative-return)))))))





;; this is more general and returns a list
;; it is intended to find all commands matching some property or something like that
(defun find-commands-in-table (test &optional (table (active-command-table)))
  (let ((results nil))
    (apply-with-command-table-inheritance
     (lambda (table)
       (loop for command in (commands table)
            when (funcall test command)
          do (push command results)))
     table)
    results))

(defun find-command-in-table (name table &key (key 'command-symbol-name) (test 'eq))
  (apply-with-command-table-inheritance
   (lambda (table)
     (let ((command (find name (commands table) :key key :test test)))
       (when command
         (return-from find-command-in-table command))))
   table)
  ;; should I error in this case?
  nil)


(defmethod find-command ((name symbol) (table command-table))
  (find-command-in-table name table :key 'command-symbol-name :test 'eq))

(defmethod find-command ((name string) (table command-table))
  (find-command-in-table name table :key 'command-name :test 'string-equal))

(defmethod find-command ((command list) (table command-table))
  (find-command (first command) table))

;; some functions are redundant now that I have a parameterised type for command-table-entry
;; removed: commands-with-type-signature
;; removed: values-satisfy-parameters, commands-for-parameters


(defmethod remove-command (property (table command-table)
                           &key (key 'command-symbol-name) (test 'eq))
  (setf (commands table)
        (remove-if (lambda (entry)
                     (funcall test entry property))
                   (commands table)
                   :key key)))


(defmethod add-command ((command command-table-entry) (table command-table))
  ;; remove it if it already exists (maybe show a warning)
  ;; - this shouldn't normally happen
  (remove-command (command-symbol-name command) table)
  (push command (commands table)))


;; Super crude type inference!
(defun infer-return-type (expression)
  ;; This can't be defined here
  ;; If I want it back I'll have to put it elsewhere...
  #+nil(cond ((and (listp expression)
                   (member (first expression)
                           '(mpresent with-web-monad mhtml)))
              'vip::web-monad)
             ;; don't know
             (t t))
  (declare (ignore expression))
  t)

;; (infer-return-type '(+ 1 2 3))
;; (infer-return-type '(mpresent something))


;; now I can define commands like this...

;; This is supposed to handle testing pre and post conditions
;; it should, no doubt, also generate a DB transaction when we get to that.
;; ... TBC
(defmacro define-command ((symbol-name &key command-table (name (error "Please specify a command name"))
                                       options
                                       return-type)
                                         parameters
                          &body body)
  `(progn
     (defun ,symbol-name ,(mapcar (lambda (x)
                                    (if (listp x)
                                        (first x) x))
                                  parameters)
       ,@body)
     (add-command (make-instance 'command-table-entry
                                 :action ',symbol-name
                                 #+nil(lambda ,(mapcar (lambda (x)
                                                         (if (listp x)
                                                             (first x) x))
                                                       parameters)
                                        ,@body)
                                 :parameters (quote ,parameters)
                                 :name ,name
                                 :symbol-name ',symbol-name
                                 :return-type ,(or return-type
                                                   (list 'quote (infer-return-type (first (last body)))))
                                 ;; :options (list :symbol-name ',symbol-name ,@options)
                                 :options ,options
                                 )
                  ,command-table)))


;; now, we can construct command objects and execute them against command tables...

(defclass command-event ()
  ((command :initarg :command :reader command)
   (command-string :initarg :command-string :reader command-string)
   (command-table-entry :initarg :command-table-entry :reader command-table-entry)))

(defclass will-execute-command (event:event command-event) ())
(defclass did-execute-command (event:event command-event)
  ((elapsed-time :initarg :elapsed-time :reader elapsed-time)))

(defclass will-execute-command-handler (event:handler) ())
(defclass did-execute-command-handler (event:handler) ())

(defmethod event:handle ((e will-execute-command) (h will-execute-command-handler)))
(defmethod event:handle ((e did-execute-command) (h did-execute-command-handler)))



(defmethod execute-command ((command list) &key (table (active-command-table)))
  ;; this is really going to be a lot like eval
  ;; it will be safe to do from untrusted sources though
  (let* ((args (cdr command))
         (last-arg (first (last args)))
         (cte (find-command (first command) table))
         ;; It's a shame to have to turn it back into a string, but I need it for events
         (cmd (present-to-string command 'command))
         (now (ccl:current-time-in-nanoseconds)))

    (event:emit 'will-execute-command
                :command command
                :command-string cmd
                :command-table-entry cte)
    
    ;; Don't execute incomplete commands
    (when (incomplete-command-p command)
      (error "Incomplete command. Expected parameter of type ~A"
             (second last-arg)))

    ;; now find the command definition...
    (let ((result (apply (command-action (or cte
                                             (error "Command not found: ~A" (first command))))
                         args)))

      (event:emit 'did-execute-command
                  :elapsed-time (- (ccl:current-time-in-nanoseconds)
                                   now)
                  :command command
                  :command-string cmd ; won't change if args of command are mutated
                  :command-table-entry cte)
      result)))

(defmethod execute-command ((command string) &key (table (active-command-table)))
  (execute-command (accept-from-string `(command :table ,table) command)
                   :table table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Where this all becomes interesting is in the parsing of command strings and
;; the generating of bits of UI...


;; !!! the parameters-type doesn't necessarily match what the command table entry has
;; (since we might transform them - see below)
(defun present-command (object stream view table &key
                                                   (parameter-transformer (lambda (x) x))
                                                   acceptably)
  ;; !!! It does not make sense to present a command in an unacceptably way
  ;; - it would not make sense
  (declare (ignore acceptably))
  (let* ((tuble (resolve-command-table table))
         (entry (find-command (first object) tuble)))
    (format stream (if (command-parameters entry)
                       "~A " "~A")
            (command-name entry))
    ;; present all the parameters presenting them properly
    ;; this will be great if triggered when space is pressed in a minput field...
    (present (cdr object) (funcall parameter-transformer
                                   (command-parameters-type entry))
             :stream stream :view view
             :acceptably t
             ;; acceptably
             )))

(define-presentation-method present ((object list) (type command) (stream stream)
                                     (view textual-view) &key acceptably)
  (present-command object stream view table :acceptably acceptably))

;; (present '(1 2 3) '(parameter-list integer (currency :gbp) integer))

(define-presentation-method present ((object list) (type command-with-unevaluated-parameters)
                                     (stream stream) (view textual-view) &key acceptably)
  (present-command object stream view table
                   :parameter-transformer #'unevaluated-parameter-list-type
                   :acceptably acceptably))




(defun incomplete-command-p (command)
  (and (listp command)
       (find *incomplete-command-marker* command
             :key (lambda (x)
                    (when (listp x)
                      (first x))))))


(defmethod command-return-type ((x list))
  (command-return-type (find-command x (active-command-table))))


(defun add-incomplete-markers (command)
  (let* ((entry (find-command command (active-command-table)))
         (ptype (command-parameters-type entry)))
    (with-presentation-type-decoded (name params)
        ptype
      (declare (ignore name))
      (append command
              (mapcar (lambda (x)
                        (list *incomplete-command-marker* x))
                      (nthcdr (length (cdr command)) params))))))




(defmacro define-presentation-translator (name (from-type to-type command-table) arglist &body body)
  `(setf (gethash ',name (clim-internals::presentation-translators ,command-table))
         (list ',from-type
               ',to-type
               (lambda ,arglist
                 ,@body))))


(defun find-presentation-translators-for-object (object type
                                                 command-table)
  (let ((results nil))
    (apply-with-command-table-inheritance
     (lambda (table)
       (loop for v being the hash-values
          in (presentation-translators table)
          for from-type = (first v)
          ;; do (break)
          when (and (presentation-typep object from-type)
                    (presentation-subtypep type
                                           (type-for-subtype-tests from-type)))
          do (push v results)))
                                          command-table)
    results))

(defun all-possible-translations (object type command-table)
  (labels ((translations (object* translator trail)
             (ignorable-destructuring-bind (_ to fn) translator
               (unless (member to trail)
                 (let* ((translated (funcall fn object*))
                        (chain (translations-trail translated to trail))
                        (translation (list translated to))) 
                   (if chain
                       (cons translation chain)
                       (list translation))))))
           
           (translations-trail (object* type* trail)
             (let ((translators (find-presentation-translators-for-object object* type* command-table)))
               (mapcan (lambda (translator) 
                         (translations object* translator (cons type* trail))) 
                       translators))))

    (translations-trail object type '())))


;; These will always be available - even when not logged in
(defparameter *base-command-table*
  (make-instance 'command-table :name "Base Commands"))





(defun make-command (name &rest parameters)
  (let ((parameters (loop for (a b) on parameters by #'cddr
                         collect (cons a b))))
    (flet ((test (a b)
             (or (eql a b)
                 (and (not (numberp b))
                      (not (numberp a))
                      (string-equal a b)))))
      (awhen (find-command name *active-command-table*)
        (cons (command-symbol-name it)
              (loop for (name type) in (command-parameters it)
                 for index from 0
                 for value = (cdr (or (assoc name parameters
                                             :test #'test)
                                      (assoc index parameters)))
               
                 for default = (unless value
                                 (cdr (or (assoc (format nil "DEFAULT-~A" name)
                                                 parameters :test #'test)
                                          (assoc (format nil "DEFAULT-~A" index)
                                                 parameters :test #'test))))
                 collect (cond (value
                                (or (if (presentation-typep value type)
                                        value
                                        (let ((possibles
                                               (all-possible-translations value
                                                                          (presentation-type-of value)
                                                                          *active-command-table*)))
                                          (awhen (find type
                                                       possibles
                                                       :key #'second
                                                       :test #'presentation-subtypep)
                                            (first it))))
                                    ;; If a value is supplied but its type is wrong then we return no command
                                    (return-from make-command nil)))
                               (default
                                (list *incomplete-command-marker*
                                      (with-presentation-type-decoded (name params options)
                                          type
                                        `((,name ,@params) ,@options :default ,default))))
                               (t (list *incomplete-command-marker*
                                        type)))))))))


