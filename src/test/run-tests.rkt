#lang racket
(require rackunit
         rackunit/text-ui
         "rabbit-db-test.rkt"
         "rabbit-test.rkt")

(run-tests rabbit-db-tests)
(run-tests rabbit-tests)