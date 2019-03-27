#! /usr/bin/env racket

#lang racket

(require
 racket/match)

(define (complement f)
  (lambda args (not (apply f args))))

(define (flatten-1 list)
  (append-map (lambda (x) x) list))

(define (tree-heads predicate tree)
  (if (not (pair? tree))
      '()
      (append (cond ((pair? (car tree))
                     (tree-heads predicate (car tree)))
                    ((predicate (car tree))
                     (list tree))
                    (else '()))
              (tree-heads predicate (cdr tree)))))

(define (map-tree-strings f tree)
  (cond ((string? tree) (f tree))
        ((not (pair? tree)) tree)
        (else (cons (map-tree-strings f (car tree))
                    (map-tree-strings f (cdr tree))))))

(define (match-char? k char)
  (cond ((procedure? k) (not (not (k char))))
        ((char? k) (equal? k char))
        (else #f)))

(define (read-char? k)
  ;;(fprintf (current-error-port) "read-char? ~a~%" k)
  (and (match-char? k (peek-char))
       (begin (let ((char (read-char)))
                ;;(display char (current-error-port))
                ;;(newline (current-error-port))
                char))))

(define (read-char* k)
  (let* ((first-char (read-char? k))
         (chars (with-output-to-string
                  (lambda ()
                    (let loop ((char first-char))
                      (unless (or (false? char) (eof-object? char))
                        (write-char char)
                        (loop (read-char? k))))))))
    (if (= 0 (string-length chars)) #f chars)))

(define (tex-command-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)))

(define (tex-special-char? ch)
  (or (equal? ch #\{)
      (equal? ch #\})
      (equal? ch #\\)))

(define (read-tex-command-args)
  (let loop ((args '()))
    (if (not (read-char? #\{))
        args
        (loop (append args (list (read-tex-until #\})))))))

(define (read-tex-thing)
  (cond ((read-char? #\\)
         (let ((command (read-char* tex-command-char?)))
           (cond (command
                  (cons (string->symbol command)
                        (read-tex-command-args)))
                 (else
                  (read-char* (complement tex-special-char?))))))
        ((read-char? #\{)
         (cons 'math (read-tex-until #\})))
        (else (read-char* (complement tex-special-char?)))))

(define (read-tex-until sentinel)
  (let loop ((things '()))
    (if (read-char? sentinel)
        things
        (let ((thing (read-tex-thing)))
          (cond ((not thing)
                 things)
                (else
                 ;;(fprintf (current-error-port) "Read thing: ~a~%" thing)
                 (loop (append things (list thing)))))))))

(define (read-tex-from-port char-input-port)
  (parameterize ((current-input-port char-input-port))
    (read-tex-until eof-object?)))

(define (parse-tex-file tex-file)
  (call-with-input-file tex-file read-tex-from-port))

(define (rnrs-tex-files rnrs)
  (sort (directory-list rnrs #:build? #t) path<?))

(define (write-rnrs-protos rnrs)
  (for-each (lambda (proto)
              (match-let (((list tex-proto-command
                                 (list name)
                                 args
                                 (list kind))
                           proto))
                (set! kind (cond ((equal? "procedure" kind) 'procedure)
                                 ((equal? '(exprtype) kind) 'syntax)
                                 (else (error "Unknown kind"))))
                (writeln
                 `(,kind
                   ,name
                   ,@(append-map
                      (lambda (the-arg)
                        (let ((the-arg
                               (remove* '("")
                                        (flatten (map-tree-strings string-trim
                                                                   the-arg)))))
                          (cond ((null? the-arg) '())
                                ((string? (first the-arg))
                                 (map (lambda (x) `(arg ,x))
                                      (if (equal? 'procedure kind)
                                          (string-split (first the-arg))
                                          (list (first the-arg)))))
                                (else
                                 (let ((las (last the-arg)))
                                   (if (or (equal? 'dots las)
                                           (equal? 'dotsfoo las))
                                       `((arg "..." rest))
                                       `((arg ,las))))))))
                      (remove* '("") args))))))
            (tree-heads (lambda (head)
                          (or (equal? 'proto head)
                              (equal? 'rproto head)))
                        (flatten-1 (map parse-tex-file
                                        (rnrs-tex-files rnrs))))))

(define (write-rnrs-protos-into-args-file rnrs)
  (call-with-atomic-output-file
   (string-append rnrs "-args.scm")
   (lambda (out . _)
     (parameterize ((current-output-port out))
       (write-rnrs-protos rnrs)))))

(write-rnrs-protos-into-args-file "r6rs")
(write-rnrs-protos-into-args-file "r7rs")
