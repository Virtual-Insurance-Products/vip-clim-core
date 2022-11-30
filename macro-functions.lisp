
(in-package :climwi)

;; functions needed by macros...


;; !!! Should error if not found
(defun presentation-method-internal-name (name)
  (intern (format nil "%~A" name) :climwi))

;; (presentation-method-internal-name 'accept)


(defun expand-with-presentation-type-bits (parameters accessor type body)
  ;; there may not BE a typespec, which means the type isn't defined as a presentation type, and therefore has no parameters. That's fine - hopefully it's a CLOS class
  (if parameters
      `(destructuring-bind ,parameters
           ;; !!! This should maybe be improved slightly
           (,accessor ,type)
         (declare (ignorable ,@(get-all-params parameters)))
         ,@body)
      ;; this will catch CLOS classes where there is no information. For the options passed just put them all into other-options
      (if (eql accessor '%typespec-options)
          `(destructuring-bind (&rest other-options)
               (,accessor ,type)
             (declare (ignorable other-options))
             ,@body)
          `(progn ,@body))))




(defmethod type-arg-position (name)
  (cond ((member name '(accept default-view-for-accepting presentation-subtypep
                        type-for-subtype-tests)) 0)
        ((member name '(present presentation-typep default-view-for-object))
         1)
        (t (error "Unknown presentation method: ~A" name))))

;; (type-arg-position 'present)
;; (type-arg-position 'accept)


;; These are methods with certain special parameters which I don't want to have to keep repeating

;; I don't know why this looks exactly like it does, but it doesn't really matter
;; !!! UNUSED
(defun type-key-name (typespec)
  (intern (format nil "(presentation-type ~A)" (%typespec-name typespec)) :climwi))
;; (type-key-name 'integer)

(defconstant standard-lambda-list-keywords 
  '(&optional &rest &aux &key &allow-other-keys &body &environment &whole))

;; This is just lifted from McCLIM's definition
;; (which is GPLd, so if we were to distribute this we would have to replace this or honor the GPL)
(defun get-all-params (ll)
  (unless ll
    (return-from get-all-params nil))
  (when (atom ll)
    (return-from get-all-params (list ll)))
  (let ((state 'required))
    (loop for arg in ll
       append (cond ((member arg standard-lambda-list-keywords :test #'eq)
		     (setq state arg)
		     nil)
		    ((eq state 'required)
		     (get-all-params arg))
		    ((or (eq state '&optional) (eq state '&aux))
		     (if (atom arg)
			 (list arg)
			 (get-all-params (car arg))))
		    ((eq state '&key)
		     (cond ((atom arg)
			    (list arg))
			   ((atom (car arg))
			    (list (car arg)))
			   (t (get-all-params (cadar arg)))))
		    ((member state '(required &rest &body &whole))
		     (when (eq state '&whole)
		       (setq state 'required))
		     (get-all-params arg))
		    (t nil)))))

;; (get-all-params '(a b &optional c d))


(defun %typespec-name (spec)
  (if (consp spec)
      (if (consp (first spec))
          (first (first spec))
          (first spec))
      spec))

(defun %typespec-presentation-class-name (spec)
  (intern (concatenate 'string (symbol-name (%typespec-name spec)) "-TYPE") :climwi))

;; (%typespec-presentation-class-name '(integer 1 10))


(defun %typespec-options (spec)
  (if (and (consp spec)
           (consp (first spec)))
      (cdr spec)))

;; (%typespec-options 'integer)
;; (%typespec-options '(integer 1 10))
;; (%typespec-options '((integer) :label "Number"))





(defparameter *type-definitions* (make-hash-table))


;; !!! These 2 work on type definitions
(defun type-definition-parameters (type-name)
  (first (gethash type-name *type-definitions*)))
(defun type-definition-options (type-name)
  (second (gethash type-name *type-definitions*)))

;; (type-definition-parameters 'list)
;; (type-definition-options 'thing)

