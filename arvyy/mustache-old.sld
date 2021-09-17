(define-library
  (arvyy mustache)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 1))
  (export compile
          execute)
  (include "mustache-impl.scm"))
