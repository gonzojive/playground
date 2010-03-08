(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :yacc)
  (require :alexandria))

(defpackage :grammars
    (:use :cl :alexandria :yacc))

(in-package :grammars)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))
     
  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b))

(defun list-lexer (list)
  #'(lambda ()
      (let ((value (pop list)))
        (if (null value)
            (values nil nil)
            (let ((terminal
                   (cond ((member value '(+ - * / |(| |)|)) value)
                         ((integerp value) 'int)
                         ((symbolp value) 'id)
                         (t (error "Unexpected value ~S" value)))))
              (values terminal value))))))

(define-parser *expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
     
  (expression
   (expression + expression #'i2p)
   (expression - expression #'i2p)
   (expression * expression #'i2p)
   (expression / expression #'i2p)
   term)
     
  (term
   id
   int
   (- term)
   (|(| expression |)| #'k-2-3)))