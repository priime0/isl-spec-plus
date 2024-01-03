#lang racket

(require (for-syntax syntax/parse racket/syntax))
(require (for-syntax (only-in racket/syntax-srcloc syntax-srcloc)
                     (only-in racket/string string-replace)))
(require (only-in test-engine/test-engine add-test!
                                          add-failed-check!
                                          failed-check
                                          property-fail
                                          property-error
                                          run-tests!
                                          test-object-failed-checks))
(require test-engine/srcloc)
(require (only-in deinprogramm/quickcheck/quickcheck
                  make-result))
(require (except-in quickcheck make-result ==>))
(require (prefix-in rkt: (only-in racket/base write)))

(module reader syntax/module-reader
  isl-spec-plus)

(require (except-in isl-spec define let check-property lambda))

(provide (all-from-out isl-spec))
(provide write define let lambda)
(provide check-property)

(define (write datum [out (current-output-port)])
  (unless (void? datum)
    (rkt:write datum out))
  (define tests (run-tests!))
  (define failed-checks (test-object-failed-checks tests))
  (when (not (null? failed-checks))
    (error 'check-property
           (string-append "failed tests:\n"
                          (string-join (map render-failed-check failed-checks) "\n")))))

(define (render-failed-check fc)
  (match-define [failed-check reason _] fc)
  (format "Failed:\n~s" reason))

(define (do-check-property srcloc prop)
  (add-test!
   (lambda ()
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (add-failed-check! (failed-check (property-error srcloc e)
                                                         (exn-srcloc e))))])
       (call-with-values
        (lambda ()
          (quickcheck-results prop))
        (lambda (ntest stamps result)
          (if (result? result)
              (begin
                (add-failed-check!
                 (failed-check (property-fail srcloc (make-result (result-ok result)
                                                                  (result-stamp result)
                                                                  (result-arguments-list result)))
                               srcloc))
                #f)
              #t)))))))

(define-syntax (check-property stx)
  (syntax-case stx ()
    ((_ prop) #`(do-check-property #,(syntax-srcloc stx) prop))
    (_ (raise-syntax-error #f "`check-property' expects a single operand" stx))))
