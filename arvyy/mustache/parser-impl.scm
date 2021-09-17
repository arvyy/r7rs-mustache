(define-record-type <interp>
                    (interp ref escape?)
                    interp?
                    (ref interp-ref)
                    (escape? interp-escape?) ;; should html be escaped
                    )

(define-record-type <section>
                    (section ref invert? content raw-content)
                    section?
                    (ref section-ref)
                    (invert? section-invert?) ;; normal section if false, {{^ section if true
                    (content section-content) ;; compiled inner content
                    (raw-content section-raw-content) ;; uncompiled inner content as a string; used for lambdas
                    )

(define-record-type <partial>
                    (partial name indent)
                    partial?
                    (name partial-name)
                    (indent partial-indent))

(define-record-type <newline>
                    (new-line content)
                    new-line?
                    (content new-line-content))

(define (parse tokens)
  (let* ((tokens (replace-standalone tokens))
         (tokens (remove-non-visible tokens))
         (tokens (convert-string-tokens tokens))
         (tokens (parse-interp+sections tokens)))
    (display (tpl->string tokens))
    (newline)
    tokens))

(define (tpl->string tokens)
  (define (->string item out)
    (cond
      ((string? item) (write-string item out))
      ((new-line? item) (write-string (new-line-content item) out))
      ((section? item)
       (let ((tagname (list->tagname (section-ref item))))
         (write-string (if (section-invert? item) "{{^" "{{#") out)
         (write-string tagname out)
         (write-string "}}" out)
         (for-each
           (lambda (item*)
             (->string item* out))
           (section-content item))
         (write-string "{{/" out)
         (write-string tagname out)
         (write-string "}}" out)))
      ((interp? item)
       (let ((tagname (list->tagname (interp-ref item))))
        (write-string (if (interp-escape? item) "{{" "{{&") out)
        (write-string tagname out)
        (write-string "}}" out)))))
  (define out (open-output-string))
  (for-each
    (lambda (item) (->string item out))
    tokens)
  (get-output-string out))

(define (standalone/remove? token)
  (or (token-comment? token)
      (token-delimchager? token)))

(define (standalone/trim? token)
  (or (token-section-open? token)
      (token-section-close? token)))

(define (replace-standalone tokens)
  (let loop ((tokens (cons (token-nl '(#\newline)) tokens))
             (result/inv '())
             (first #t)
             (prev-standalone #f))
    (cond
      ((null? tokens) (reverse result/inv))
      
      ;; tokens to remove
      ((or (match-follows tokens token-nl? token-ws? standalone/remove? token-ws? token-nl?)
           (match-follows tokens token-nl? token-ws? standalone/remove? token-nl?)) =>
       (lambda (tokens*)
         ;; retain initial newline
         (loop (if (or first ) 
                   tokens*
                   (cons (car tokens) tokens*))
               result/inv
               #f
               #t)))
      
      ;; tokens to remove
      ((or (match-follows tokens token-nl? standalone/remove? token-ws? token-nl?)
           (match-follows tokens token-nl? standalone/remove? token-nl?)) =>
       (lambda (tokens*)
         ;; retain initial newline
         (loop (if (or first ) 
                   tokens*
                   (cons (car tokens) tokens*))
               result/inv
               #f
               #t)))
      
      ;; tokens to remove when it's a last line
      ((or (match-follows tokens token-nl? token-ws? standalone/remove? token-ws? eof-object?)
           (match-follows tokens token-nl? token-ws? standalone/remove? eof-object?)
           (match-follows tokens token-nl? standalone/remove? token-ws? eof-object?)
           (match-follows tokens token-nl? standalone/remove? eof-object?)) =>
       (lambda (tokens*)
         ;; retain initial newline
         (loop '()
               (if (or first )
                   result/inv
                   (cons (car tokens)
                         result/inv))
               #f 
               #t)))
      
      ;; tokens to trim
      ((or (match-follows tokens token-nl? token-ws? standalone/trim? token-ws? token-nl?)
           (match-follows tokens token-nl? token-ws? standalone/trim? token-nl?)) => 
       (lambda (tokens*)
         ;; retain initial newline and the trimmed token
         (loop (if (or first)
                   tokens*
                   (cons (car tokens) tokens*))
               (append (list (caddr tokens))
                       result/inv)
               #f
               #t)))
      
      ((or (match-follows tokens token-nl? standalone/trim? token-ws? token-nl?)
           (match-follows tokens token-nl? standalone/trim? token-nl?)) => 
       (lambda (tokens*)
         ;; retain initial newline and the trimmed token
         (loop (if (or first)
                   tokens*
                   (cons (car tokens) tokens*))
               (append (list (cadr tokens))
                       result/inv)
               #f
               #t)))
      
      ;; tokens to trim when it's a last line
      ((or (match-follows tokens token-nl? token-ws? standalone/trim? token-ws? eof-object?)
           (match-follows tokens token-nl? token-ws? standalone/trim? eof-object?)) => 
       (lambda (tokens*)
         ;; retain initial newline and the trimmed token
         (loop '()
               (append (list (caddr tokens))
                       (if (or first ) '() (list (car tokens)))
                       result/inv)
               #f
               #t)))
      
      ((or (match-follows tokens token-nl? standalone/trim? token-ws? eof-object?)
           (match-follows tokens token-nl? standalone/trim? eof-object?)) => 
       (lambda (tokens*)
         ;; retain initial newline and the trimmed token
         (loop '()
               (append (list (cadr tokens))
                       (if (or first ) '() (list (car tokens)))
                       result/inv)
               #f
               #t)))
      
      ;; token for partial, remembering the indentation
      ((or (match-follows tokens token-nl? token-ws? token-partial? token-ws? token-nl?)
           (match-follows tokens token-nl? token-ws? token-partial? token-nl?)) =>
       (lambda (tokens*)
         (loop tokens*
               (append (list (partial (token-partial-tag (caddr tokens))
                                      (token-ws-count (cadr tokens))))
                       (if (or first ) '() (list (car tokens)))
                       result/inv)
               #f
               #t)))
      
      ((or (match-follows tokens token-nl? token-partial? token-ws? token-nl?)
           (match-follows tokens token-nl? token-partial? token-nl?)) =>
       (lambda (tokens*)
         (loop tokens*
               (append (list (partial (token-partial-tag (cadr tokens))
                                      0))
                       (if (or first ) '() (list (car tokens)))
                       result/inv)
               #f
               #t)))
      
      ;; token for partial, remembering the indentation, when on last line
      ((or (match-follows tokens token-nl? token-ws? token-partial? token-ws? eof-object?)
           (match-follows tokens token-nl? token-ws? token-partial? eof-object?)) =>
       (lambda (tokens*)
         (loop '()
               (append 
                 (list (partial (token-partial-tag (caddr tokens))
                                      (token-ws-count (cadr tokens))))
                       (if (or first ) '() (list (car tokens)))
                       result/inv) 
               #f
               #t)))
      
      ((or (match-follows tokens token-nl? token-partial? token-ws? eof-object?)
           (match-follows tokens token-nl? token-partial? eof-object?)) =>
       (lambda (tokens*)
         (loop '()
               (append (list (partial (token-partial-tag (cadr tokens))
                                      0))
                       (if (or first ) '() (list (car tokens)))
                       result/inv)
               #f
               #t)))
      
      ((match-follows tokens token-partial?) => (lambda (tokens*)
                                                  (loop tokens*
                                                        (cons (partial (token-partial-tag (car tokens))
                                                                       0)
                                                              result/inv)
                                                        #f
                                                        #f)))
      
      (else (loop (cdr tokens)
                  (if (or first)
                      result/inv
                      (cons (car tokens) result/inv))
                  #f
                  #f)))))

(define (convert-string-tokens tokens)
  (let loop ((tokens tokens)
             (out #f)
             (result/inv '()))
    (cond
      ((null? tokens)
       (let ((result-final/inv (if out
                                   (cons (get-output-string out)
                                         result/inv)
                                   result/inv)))
         (reverse result-final/inv)))
      ((or (token-str? (car tokens))
           (token-ws? (car tokens)))
       (let* ((token (car tokens))
              (out* (if out 
                        out
                        (open-output-string)))
              (str (if (token-str? token)
                       (token-str-content token)
                       (make-string (token-ws-count token) #\space))))
         (write-string str out*)
         (loop (cdr tokens)
               out*
               result/inv)))
      (else (let* ((token (car tokens))
                   (value (cond
                            ((token-nl? token) (new-line (list->string (token-nl-chars token))))
                            (else token)))
                   (new-result/inv (if out
                                       (cons (get-output-string out)
                                             result/inv)
                                       result/inv)))
              (loop (cdr tokens)
                    #f
                    (cons value new-result/inv)))))))

(define (parse-interp+sections tokens)
  (define (parse-interp+sections* tokens expected-close-tag)
    (let loop ((tokens tokens)
               (result/inv '()))
      (cond
        ((null? tokens)
         (if expected-close-tag
             (error "Unexpected eof")
             (values '() (reverse result/inv))))
        ((token-section-close? (car tokens))
         (if (equal? expected-close-tag (token-section-close-tag (car tokens)))
             (values (cdr tokens) (reverse result/inv))
             (error "Closing token mismatch")))
        ((token-section-open? (car tokens))
         (let* ((token (car tokens))
                (tag (token-section-open-tag token))
                (ref (tagname->list tag)))
          (define-values (tokens* result*)
                         (parse-interp+sections* (cdr tokens)
                                                 tag))
          (define value (section ref 
                                 (token-section-open-inverted? token)
                                 result*
                                 #f))
          (loop tokens*
                (cons value result/inv))))
        ((token-interp? (car tokens))
         (let* ((token (car tokens))
                (tag (token-interp-tag token))
                (ref (tagname->list tag)))
           (define value (interp ref (token-interp-escape? token)))
           (loop (cdr tokens)
                 (cons value result/inv))))
        (else (loop (cdr tokens)
                    (cons (car tokens)
                          result/inv))))))
  (define-values (tokens* result)
                 (parse-interp+sections* tokens #f))
  result)

(define (remove-non-visible tokens)
  (filter
    (lambda (token)
      (not (or (token-comment? token)
               (token-delimchager? token))))
    tokens))

(define (match-follows in . preds)
  (let loop ((in* in)
             (preds* preds))
    (cond
      ((null? preds*) in*)
      ((null? in*) (and (null? (cdr preds*))
                        (eq? (car preds*) eof-object?)
                        '()))
      (((car preds*) (car in*)) 
       (loop (cdr in*)
             (cdr preds*)))
      (else #f))))

(define (tagname->list str)
  (define (prepend-part parts part)
    (when (null? part)
      (error "Trailing period in tag name"))
    (cons (list->string (reverse part))
          parts))
  (if (equal? "." str)
      '(".")
      (let loop ((in (string->list str))
                 (parts '())
                 (part '()))
        (cond
          ((null? in)
           (reverse (prepend-part parts part)))
          ((char=? #\. (car in))
           (loop (cdr in)
                 (prepend-part parts part)
                 '()))
          (else (loop (cdr in)
                      parts
                      (cons (car in) part)))))))

(define (list->tagname lst)
  (apply string-append
         (cdr (apply append
                     (map
                       (lambda (el) (list "." el))
                       lst)))))
