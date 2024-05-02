;;;; ldemo.asd

(asdf:defsystem #:ldemo
  :description "Describe ldemo here"
  :author "Ryan Burnside"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str
               #:nodgui)
  :components ((:file "package")
               (:file "turt")
               (:file "ldemo")))
