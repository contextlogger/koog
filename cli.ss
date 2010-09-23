#lang scheme

;; This file implements the command-line interface of Koog.

(require scheme/port)
(require scheme/cmdline)
(require "koog.ss")

(provide main)

(define (main)
  ;; (write-nl (find-system-path 'run-file)) ;; program name
  ;; (write-nl (current-command-line-arguments)) ;; args

  (define from-stdin (make-parameter #f)) ;; #f ==> from file
  (define out-stream (make-parameter #f)) ;; #f ==> to file
  (define files #f)
  (define stdin-filename (make-parameter #f)) ;; #f means none or unknown
  
  (command-line
   #:program (find-system-path 'run-file) ;; program name
   #:once-each
   (("-d" "--diff")     "print a diff to STDOUT"
    (diff-stream (current-output-port)))
   (("-f" "--filename") filename  "filename for when from STDIN"
    (stdin-filename filename))
   (("-i" "--stdin")    "read input from STDIN (when no files specified)"
    (from-stdin #t))
   (("-n" "--null")     "print expanded input to /dev/null only"
    (out-stream (open-output-nowhere)))
   (("-o" "--stdout")   "print expanded input to STDOUT only"
    (out-stream (current-output-port)))
   (("-l" "--line") integer  "expand section containing line only"
    (only-on-line (string->number integer)))
   (("-q" "--quiet")    "be quiet"
    (be-quiet? #t))
   (("-r" "--remove")   "remove markers"
    (remove-markers? #t))
   (("-s" "--simulate") "short for -dnq"
    (begin
      (be-quiet? #t)
      (diff-stream (current-output-port))
      (out-stream (open-output-nowhere))))
   #:args file (set! files file))
  
  ;;(write-nl (list files (only-on-line)))

  (if (from-stdin)
      (koog-expand (current-input-port)
                    (or (out-stream) (current-output-port))
                    (stdin-filename))
      (for-each
       (lambda (filename)
         (koog-expand #f (out-stream) filename))
       files))

  ;; We want this as the --eval appears to print out the eval result.
  ;; This produces a result that is nothing, and hence nothing gets
  ;; printed.
  (void)
  ) ;; end main
