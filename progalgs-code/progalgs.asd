(in-package #:asdf-user)

(defsystem #:progalgs
  :version "1.1"
  :description "Code for the book 'Programming Algorithms in Lisp'"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :depends-on (#:rutils #:eager-future2 #:sha1 #:lparallel #:should-test)
  :serial t
  :components ((:file "package")
               (:file "ch01-complexity")
               (:file "ch04-data-structures")
               (:file "ch05-arrays")
               (:file "ch06-lists")
               (:file "ch07-kvs")
               (:file "ch08-hash-tables")
               (:file "ch09-trees")
               (:file "ch10-graphs")
               (:file "ch11-strings")
               (:file "ch12-dynamic-programming")
               (:file "ch13-approximation")
               (:file "ch14-compression")
               (:file "ch15-synchronization")))
