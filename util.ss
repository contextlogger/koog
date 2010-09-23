#lang scheme

;; Utilities used internally in the implementation of Koog. Should be
;; considered a non-public API, subject to change at any time.

(provide define* define-syntax*)

(define-syntax define*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define name body ...)
       (provide name)))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name stx) body ...)
     (begin
       (define-syntax (name stx) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define-syntax name body ...)
       (provide name)))))

(define-syntax* thunk
  (syntax-rules ()
    ((_ body ...)
     (lambda () body ...))))

(define* write-nl
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define* display-nl
  (case-lambda
    ((datum) (begin (display datum) (newline)))
    ((datum out) (begin (display datum out) (newline out)))))

;; "Transfers" data from an input stream into an output stream.
;; 
;; Code from Swindle served as an example for this.
(define (read-write in out)
  (let* ((buffer (make-bytes 4096)))
    (let loop ()
      (let ((num-bytes (read-bytes-avail! buffer in)))
        (unless (eof-object? num-bytes)
          (write-bytes buffer out 0 num-bytes)
          (loop))))))

(define* (read-string-from-file file)
  (let* ((out (open-output-string)))
    (call-with-input-file
        file
      (lambda (in)
        (read-write in out)))
    (get-output-string out)))
