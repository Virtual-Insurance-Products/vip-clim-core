
(in-package :cl-user)

;; !!! The package definition should be moved elsewhere
(defpackage #:clim-web-internals
  (:nicknames #:climwi)
  (:use :common-lisp :cl-ppcre :vip-utils :simple-parser :anaphors)
  (:export #:define-presentation-method
           #:define-presentation-type
           #:define-presentation-translator
           #:accept
           #:accept-from-string
           #:default-view-for-accepting
           #:maccept ; this is web monad specific, but useful
           #:present 
           #:mpresent
           #:present-to-string
           #:presentation-typep
           #:presentation-subtypep
           #:presentation-type-checker
           #:with-output-as-presentation
           #:textual-view
           #:html-value-view
           #:stream-present
           #:presentation-type-of
           #:type-for-subtype-tests
           #:map-over-presentation-type-supertypes
           #:thing
           #:with-presentation-type-decoded
           #:with-presentation-type-parameters
           #:with-presentation-type-options

           #:stream-default-view
           #:default-view-for-object
           #:stream-accept


           #:typespec
           #:typespec-option
           #:typespec-option-p
           
           #:command-table-entry 
           #:command
           #:command-with-unevaluated-parameters
           #:command-table
           #:command-table-name
           #:active-command-table
           #:define-command
           #:command-name
           #:command-symbol-name
           #:execute-command
           #:command-option
           #:*base-command-table*

           #:add-incomplete-markers

           #:command-return-type
           
           #:find-commands-in-table
           #:find-command

           #:make-command

           #:define-parser-acceptor

           ;; EXPORT THE TYPE NAMES...
           ;; NOTE: I shouldn't have to export things like number, list etc
           ;; because they come from elsewhere anyway
           #:thing
           #:currency
           #:percentage
           #:uk-postcode
           #:typespec
           #:named-type
           #:parameter-list
           #:date-and-time
           #:date
           #:sequence
           
           #:??parameters
           #:element-type

           #:one-of
           #:has
           )
  ;; (:import-from fare-matcher match ematch)
  )
