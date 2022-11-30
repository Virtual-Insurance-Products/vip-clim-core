
(in-package :climwi)

(defparameter *delimiter-gestures* :eof)

;; !!! FIXME: allow-command-invocation is not passed through the string->SP->accept chain
(defun accept (type &key (stream *standard-input*)
                      (delimiter-gestures *delimiter-gestures*)
                      (allow-command-invocation t)
                      
                      (name nil) ; only really needed for web monad accept
                      (view (default-view-for-accepting type stream :name name))
                      (default (typespec-option type :default))
                      (error-if-not-eof nil))
  (let ((*delimiter-gestures* delimiter-gestures))

    (let ((accepted (or (stream-accept stream type :error-if-not-eof error-if-not-eof :view view
                                       :default default
                                       :delimiter-gestures delimiter-gestures)
                        (and allow-command-invocation
                             ;; !!! Should we be using stream-accept here directly? I don't think so
                             (awhen (accept `(command :return-type ,type)
                                            :stream stream
                                            :delimiter-gestures delimiter-gestures
                                            :allow-command-invocation nil
                                            :name name
                                            :view view
                                            :error-if-not-eof error-if-not-eof)
                               (awhen (execute-command it :table (active-command-table))
                                 (unless (presentation-typep it type)
                                   ;; !!! If I error here then that will have to be caught in some
                                   ;; situations. minput has to handle !eof errors anyway
                                   (error "~S is not of type ~S" it type))
                                 it))))))
      
      (when accepted
        ;; !!! :web-monad specific
        (if (eql stream :web-monad)
            ;; for a monadic stream the value will be a monadic value which we can't generally access
            accepted
            (if (presentation-typep accepted type)
                accepted
                ;; !!! What to do in this case probably should be controllable with a switch
                (error "Accepted object ~S is not of type ~S" accepted type)))))))


;; !!! NEEDS MORE ARGS
(defun accept-from-string (type string &key
                                         (delimiter-gestures :eof)
                                         (allow-command-invocation t)
                                         (name nil)
                                         ;; probably never used?
                                         view
                                         (error-if-not-eof nil))
  (accept type :stream string :delimiter-gestures delimiter-gestures
          :allow-command-invocation allow-command-invocation
          :name name
          :view view
          :error-if-not-eof error-if-not-eof))


(defun default-view-for-accepting (type stream &key name)
  (or (awhen (typespec-option type :accept-view)
        (if (and (listp it)
                 (symbolp (first it)))
            (apply #'make-instance it)
            it))
      (funcall-presentation-generic-function default-view-for-accepting type stream :name name)))

;; By default there is NO view and usually none is needed
;; HOWEVER, any kind of GUI stream, including the web monad one, has this concept of generating
;; a default acceptor view for input, which may be overriden.

;; It's quite possible that I won't need quite as many views for doing this stuff any more
(define-presentation-method default-view-for-accepting ((type thing) (stream t) &key name)
  name
  nil)

(define-presentation-method default-view-for-accepting ((type t) (stream t) &key name)
  type name nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; stream-accept

;; these definitions should, perhaps, be pulled out of here

;; the way stream accept works varies depending on the kind of stream we're talking about
;; in the case of web monad streams we don't need to have %accept methods defined
;; ... the view takes care of it all

;; OTOH if we wanted to abstract out the means of accepting in order to allow, say,
;; accepting input as XML vs JSON vs URI encoded VS human readable
;; maybe it makes sense to support views for other streams too
;; maybe those views are analogous? 

;; It's useful to have views for web-monad stuff for slider/text box/pastable data etc


;; This is the general behaviour of this thing
(defmethod stream-accept ((stream t) type
                          &key view error-if-not-eof
                            default
                            delimiter-gestures)
  
  ;; establishing this dynamic binding here should be the standard thing to do in stream-accept
  (let ((*delimiter-gestures* delimiter-gestures))
    (climwi::funcall-presentation-generic-function
     accept type stream view :error-if-not-eof error-if-not-eof
     :default default)))



