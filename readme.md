# Portable mustache templating implementation in R7RS

This library implements version 1.2.1 of the mustache spec. 
Implementation passes all tests in the spec.

This readme only describes specifics of this implementation. For general semantics, see https://mustache.github.io/mustache.5.html

Report issues at https://github.com/arvyy/r7rs-mustache/issues

## Base usage

`(import (arvyy mustache))`

First, templates need to be compiled, using one of following ways.

* `(compile root partial-locator)`. Here `root` should be a string of the "root" partial which will be the entry point
during execution. `partial-locator` must be a procedure, that accepts partial's name as a single argument, and returns either #f if it cannot be found, or string or textual input port. 

```
(define (locator name)
  (cond
    ((string=? name "root") "Hello {{>foo}}")
    ((string=? name "foo") "{{world}}")
    (else #f))
(define hello-world-compiled (compile "root" locator))
```

* `(compile template)`. Compiles given template string.

```
(define hello-world-compiled (compile "Hello {{world}}"))
```

Compiled template can be executed using

* `(execute compilation data out)`. `compilation` is result of `compile` invokation; `data` is the root object for interpolations; `out` is a textual output port, to which result will be writen. Returns unspecified value

```
(call-with-output-file "result.txt" 
                       (lambda (out) 
                         (execute hello-world-compiled
                                  '((world . "Scheme"))
                                   out)))
```

* `(execute compilation data)`. Instead of writing to a port, returns interpolated string result as a function value.

```
(display (execute hello-world-compiled '((world . "Scheme"))))
```

By default, vectors and streams are accepted for list interpolation, alists for object field lookup, data values are writen as with `display`.

## Behavior customization

Behavior can be altered by use of parameterization.

### Field lookup in object

Lookup is a procedure, that finds corresponding value given an object and a fragment (fragment being elements of list after splitting tag name by `.`), of the following form

`(lookup object name found not-found)`. Object is currently examined datum; name is a fragment / field of string type; found is a function that should be invoked in tail position if field was found in the object with corresponding value as an argument; not-found is a 0 argument function that should be invoked in tail position if field was not found in the object, or if the object cannot be inspected with this lookup function.

For example, creating a lookup for a record type:

```
(define-record-type <foo> (foo bar) foo? (bar foo-bar))
(define (foo-lookup obj name found not-found)
  (cond
    ((not (foo? obj)) (not-found))
    ((string=? "bar" name) (found (foo-bar obj)))
    (else (not-found))))
```

Use `compose-lookups` to merge multiple lookup implementations into one:

```
(define alist+foo (compose-lookups alist-lookup foo-lookup))
```

When composed, each lookup is tried to be applied to the object until one of lookups invokes `found`, in given order. If all lookups return not-found, then composition also returns not-found

The lookup used during execution is retrieved from `current-lookup` parameter. It defaults to `alist-lookup`.

### Collection

Collection is a reference to a set of methods of an iterable multivalue object type, which is used to expand sections. Collection is created with

`(collection pred?-proc empty?-proc for-each-proc)` where `pred?-proc` is a predicate for given collection, `empty?-proc` is procedure returning if given collection is empty, `for-each-proc` is a procedure that given a one argument function and this collection, executes given function for each element.

```
(define list-collection (collection list? null? for-each))
```

Use `compose-collections` to merge multiple collections into one

```
(define vec+list (compose-collections vector-collection list-collection))
```

The collection used during execution is retrieved from `current-collection` parameter. It defaults to `(compose-collections vector-collection stream-collection)`. While the library does export `list-collection`, it is not used by default as to not clash with alist lookup for objects.


### Value writing

Writer is a procedure of form

`(writer obj out)` where obj is a scheme object, and out is textual output port. Writer should write appropriate representation of obj to out.

The writer used during execution is retrieved from `current-writer` parameter. It defaults to `(lambda (obj out) (when obj (display obj out)))`.
