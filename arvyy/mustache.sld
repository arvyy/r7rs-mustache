(define-library
  (arvyy mustache)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (arvyy mustache lookup)
          (arvyy mustache collection)
          (prefix (arvyy mustache executor) executor-)
          (arvyy mustache parser)
          (arvyy mustache tokenizer)
          (srfi 1))
  (export execute
          compile)
  (include "mustache-impl.scm"))
