#lang scheme/base

;; This module defines the "runtime library" for Koog directives.
;; These exports will be available without an explicit "require".

(define stdout (make-parameter #f))
(define stderr (make-parameter #f))
(define rt.filename (make-parameter #f))
(define rt.section (make-parameter #f))
(define rt.directive (make-parameter #f))
(define rt.region (make-parameter #f))

(provide stdout stderr rt.filename rt.section rt.directive rt.region)
