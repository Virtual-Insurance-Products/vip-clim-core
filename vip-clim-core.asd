
;;;; clim-core.asd

(asdf:defsystem #:vip-clim-core
  :description "CLIM presentation types"
  :author "VIP"
  :license "vip"
  :depends-on ("cybertiggyr-time" "simple-parser" "cl-ppcre" "vip-utils" "anaphors" "event-handlers" "cl-json")
  ;; :serial t
  :components ((:file "package")
               (:file "macro-functions" :depends-on ("package"))
               (:file "macros" :depends-on ("macro-functions"))
               (:file "00-presentation-types" :depends-on ("macros"))
               (:file "02-accept" :depends-on ("03-commands" "00-presentation-types"))
               (:file "additional-presentation-types" :depends-on ("00-presentation-types"))
               (:file "02.1-accept-simple-parser" :depends-on ("02-accept"
                                                               "additional-presentation-types"))
               (:file "03-commands" :depends-on ("00-presentation-types"))
               (:file "03.2-command-accept" :depends-on ("03-commands"))
               ))

