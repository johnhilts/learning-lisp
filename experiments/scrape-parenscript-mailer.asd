;;;; scrape-parenscript-mailer.asd
;;;; (asdf:load-system "scrape-parenscript-mailer")

(asdf:defsystem #:scrape-parenscript-mailer
  :description "Scraper for the Parenscript Mailing List Archive"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:drakma)
  :components ((:file "scrape-parenscript-mailer")))
