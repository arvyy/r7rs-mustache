;; interface for a collection
(define-record-type <collection>
                    (collection pred-proc empty?-proc for-each-proc)
                    collection?
                    (pred-proc pred-proc)
                    (empty?-proc empty?-proc)
                    (for-each-proc for-each-proc))

(define vector-collection
  (collection vector? 
              (lambda (v) (= 0 (vector-length v)))
              vector-for-each))

(define list-collection
  (collection list?
              null?
              for-each))

(define (compose-collections . collections)
  (define (find-collection object)
    (let loop ((collections collections))
     (cond
       ((null? collections)
        #f)
       (((pred-proc (car collections)) object)
        (car collections))
       (else (loop (cdr collections))))))
  
  (collection
    ;; predicate
    (lambda (object)
      (cond
        ((find-collection object) #t)
        (else #f)))
    ;; empty proc
    (lambda (object)
      (cond
        ((find-collection object) => (lambda (c) ((empty?-proc c) object)))
        (else (error "Collection not found"))))
    ;; for-each proc
    (lambda (proc object)
      (cond
        ((find-collection object) => (lambda (c) ((for-each-proc c) proc object)))
        (else (error "Collection not found"))))))

(define-record-type <interp>
                    (interp name escape?)
                    interp?
                    (name interp-name) ;; tagname splitted by period
                    (escape? interp-escape?) ;; should html be escaped
                    )

(define-record-type <section>
                    (section name invert? content raw-content)
                    section?
                    (name section-name) ;; tagname splitted by period
                    (invert? section-invert?) ;; normal section if false, {{^ section if true
                    (content section-content) ;; compiled inner content
                    (raw-content section-raw-content) ;; uncompiled inner content as a string; used for lambdas
                    )

;; helpers during parsing
(define-record-type <comment>
                    (comment)
                    comment?)

(define-record-type <section-close>
                    (section-close name)
                    section-close?
                    (name section-close-name) ;; string of the closing tag
                    )

;; compile string into a list
;; where each element is either:
;; * string literal without any more processing needed
;; * interp
;; * section
(define (compile str)
  (compile-h (string->list str)
             '(#\{ #\{)
             '(#\} #\})
             #f))

(define (compile-h in 
                   open-delim
                   close-delim
                   expect-close-tag)
  (define (prepend-str-literal result str-literal)
    (if (null? str-literal)
        result
        (cons (list->string (reverse str-literal))
              result)))
  (let loop ((in in) ;; input as list of chars
              ;; state tracking regarding current line being 
              ;; potentially standalone tag (comment or section) 
              ;; 0: only white space seen
              ;; 1: only white space, then single standalone tag, and then only white space seen
              ;; -1 otherwise
             (standalone-state 0) 
             (str-literal '()) ;; current partial result of string literal (ie the one without processing needs) being parsed
             (result '()) ;; final result, reversed
             (open-delim open-delim) ;; open delimiter
             (close-delim close-delim)) ;; close delimiter
    
    (define (dispatch)
      (define n '(#\newline))
      (define rn '(#\return #\newline))
      (cond
        ((match-follows* in open-delim) => dispatch-open-delim)
        ((match-follows* in rn) => (lambda (in*) (dispatch-eol in* rn)))
        ((match-follows* in n) => (lambda (in*) (dispatch-eol in* n)))
        (else (dispatch-literal))))
    
    (define (dispatch-open-delim in)
      (define-values (in* el) (handle-after-open-delim in open-delim close-delim))
      (define new-state
        (if (= standalone-state 0)
            1
            -1))
      (cond
        ((comment? el) 
         (loop in* new-state str-literal result open-delim close-delim))

        ((section? el)
         (loop in*
               new-state
               '()
               (cons el (prepend-str-literal result str-literal))
               open-delim
               close-delim))

        ((section-close? el) 
         (if (equal? expect-close-tag (section-close-name el))
             (values in* (reverse (prepend-str-literal result str-literal)))
             (error "Mismatched section close tag")))

        ((interp? el)
         (loop in*
               -1
               '()
               (cons el (prepend-str-literal result str-literal))
               open-delim
               close-delim))
        
        (else (error "Unrecognized element" el))))
    
    (define (trim-spaces chars)
      (let loop ((chars chars))
       (cond
         ((null? chars) '())
         ((char=? (car chars) #\space) (loop (cdr chars)))
         (else chars))))
    
    (define (dispatch-eol in* eol-chars)
      (if (= standalone-state 1)
          (loop in*
                0
                '()
                (prepend-str-literal result (trim-spaces str-literal))
                open-delim
                close-delim)
          (loop in*
                0
                (append (reverse eol-chars) str-literal)
                result
                open-delim
                close-delim)))
    
    (define (dispatch-literal)
      (define char (car in))
      (define new-state
        (if (char=? char #\space)
            standalone-state
            -1))
      (loop (cdr in)
            new-state
            (cons char str-literal)
            result
            open-delim
            close-delim))
    
    (cond
      ((null? in)
       (cond
         (expect-close-tag (error "Unexpected EOF"))
         (else (reverse (prepend-str-literal result
                                             (if (= 1 standalone-state)
                                                 (trim-spaces str-literal)
                                                 str-literal))))))
      (else (dispatch)))))

(define (match-follows in delim)
  (let loop ((in* in)
             (delim* delim))
    (cond
      ((null? delim*) (values #t in*))
      ((null? in*) (values #f in))
      ((char=? (car in*) (car delim*)) 
       (loop (cdr in*)
             (cdr delim*)))
      (else (values #f in)))))

(define (match-follows* in delim)
  (call-with-values
    (lambda () (match-follows in delim))
    (lambda (follows? in*) (and follows? in*))))

(define (skip-spaces in)
  (cond
    ((null? in) '())
    ((char=? (car in) #\space) (skip-spaces (cdr in)))
    (else in)))

(define (html-escape writer value)
  (define str-value
    (let ((out (open-output-string)))
     (writer value out)
     (get-output-string out)))
  (define out (open-output-string))
  (string-for-each
      (lambda (char)
        (case char
          ((#\&) (write-string "&amp;" out))
          ((#\<) (write-string "&lt;" out))
          ((#\>) (write-string "&gt;" out))
          ((#\") (write-string "&quot;" out))
          (else (write-char char out))))
      str-value)
  (get-output-string out))

(define (handle-after-open-delim in open-delim close-delim)
  (define in* (skip-spaces in))
  (define next (and (not (null? in*))
                    (car in*)))
  (define rest (and (not (null? in*))
                    (cdr in*)))
  (cond
    ((null? in*) (error "Unexpected EOF"))
    ((char=? next #\!) (handle-comment rest close-delim))
    ((char=? next #\#) (handle-section rest open-delim close-delim #t))
    ((char=? next #\^) (handle-section rest open-delim close-delim #f))
    ((char=? next #\/) (handle-section-close rest close-delim))
    ((char=? next #\&) (handle-interp rest close-delim #f))
    ((char=? next #\{) (handle-interp rest (cons #\} close-delim) #f))
    (else (handle-interp in* close-delim #t))))

(define (handle-comment in close-delim)
  (define-values
    (closed? rest)
    (match-follows in close-delim))
  (cond
    ((null? in) (error "Unexpected EOF"))
    (closed? (values rest (comment)))
    (else (handle-comment (cdr in)
                          close-delim))))

(define (read-tag-and-close in close-delim)
  (define (verify+cleanup tag-chars in*)
    (define trimmed-chars (skip-spaces tag-chars))
    (for-each
      (lambda (c)
        (when (char=? c #\space)
          (error "Space inside tag value")))
      trimmed-chars)
    (values in*
            (list->string (reverse trimmed-chars))))
  (let loop ((in (skip-spaces in))
             (chars '()))
    (define-values (follows? in*)
                   (match-follows in close-delim))
    (if follows?
        (verify+cleanup chars in*)
        (loop (cdr in)
              (cons (car in) chars)))))

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

(define (handle-section in open-delim close-delim normal?)
  (define-values (in* tagname)
                 (read-tag-and-close in close-delim))
  (define name (tagname->list tagname))
  (define-values (in** content) 
                 (compile-h in* open-delim close-delim tagname))
  (define sec (section name (not normal?) content #f))
  (values in** sec))

(define (handle-section-close in close-delim)
  (define-values (in* tagname)
                 (read-tag-and-close in close-delim))
  (values in* (section-close tagname)))

(define (handle-interp in close-delim escape?)
  (define-values (in* tagname)
                 (read-tag-and-close in close-delim))
  (define name (tagname->list tagname))
  (define int (interp name escape?))
  (values in* int))

;;;; INTERPOLATION

(define (compose-lookups . lookups)
  (lambda (obj name found not-found)
    (let loop ((lookups lookups))
     (if (null? lookups)
         (not-found)
         (let ((l (car lookups)))
          (l obj name found (lambda () 
                              (loop (cdr lookups)))))))))

(define (alist-lookup obj name found not-found)
  (define key (string->symbol name))
  (define alist? (and (list? obj)
                      (or (null? obj)
                          (pair? (car obj)))))
  (if alist?
      (cond
        ((assoc key obj) => (lambda (pair) (found (cdr pair))))
        (else (not-found)))
      (not-found)))

(define (lookup-in-stack-single name objs-stack lookup)
  (let loop ((objs objs-stack))
   (if (null? objs)
       (values objs #f)
       (lookup (car objs) 
               name
               (lambda (value) (values objs value))
               (lambda () (loop (cdr objs)))))))

(define (lookup-in-stack name-lst objs-stack lookup)
  (define-values (objs value)
                 (lookup-in-stack-single (car name-lst) objs-stack lookup))
  (cond
    ((not value) #f)
    ((null? (cdr name-lst)) value)
    (else (lookup-in-stack (cdr name-lst)
                           (list value)
                           lookup))))

(define (value-writter value out)
  (when value
    (display value out)))

(define default-collection (compose-collections vector-collection))
(define default-constructor vector)

(define execute
  (case-lambda
    ((template data)
     (define out (open-output-string))
     (execute template data out)
     (get-output-string out))
    ((template data out)
     ;; TODO replace lookup, foreach, null, writter with calls to parameters
     (execute-h template (list data) out alist-lookup default-collection default-constructor value-writter))))

(define (execute-h template objs-stack out lookup collection lst-constructor writer)
  (for-each
    (lambda (fragment)
      (cond
        
        ((string? fragment)
         (write-string fragment out))

        ((interp? fragment)
         (let* ((name (interp-name fragment))
                (value (if (equal? '(".") name)
                           (car objs-stack)
                           (lookup-in-stack name
                                            objs-stack
                                            lookup))))
           (if (interp-escape? fragment)
               (write-string (html-escape writer value) out)
               (writer value out))))
        
        ((section? fragment)
         (let ((value (lookup-in-stack (section-name fragment)
                                       objs-stack
                                       lookup))
               (inner-template (section-content fragment)))
           ;; coerce value to collection
           (define value*
             (cond
               ((not value) (lst-constructor))
               ((not ((pred-proc collection) value)) (lst-constructor value))
               (else value)))
           (if (section-invert? fragment)
               (when ((empty?-proc collection) value*)
                 (execute-h inner-template objs-stack out lookup collection lst-constructor writer))
               ((for-each-proc collection)
                 (lambda (el)
                   (execute-h inner-template (cons el objs-stack) out lookup collection lst-constructor writer))
                 value*))))))
    template))
