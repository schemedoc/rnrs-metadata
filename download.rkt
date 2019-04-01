#! /usr/bin/env racket

#lang racket

(require
 file/untgz
 net/url)

(define cache-dir (build-path (current-directory) ".cache"))
(define tmp-dir (build-path (current-directory) ".tmp"))

(define (delete-tmp-files path)
  (cond ((link-exists? path) #f)
        ((and (file-exists? path)
              (equal? #".tex" (path-get-extension path)))
         (delete-file path))
        ((directory-exists? path)
         (for-each delete-tmp-files (directory-list path #:build? #t))
         (delete-directory path))))

(define (download rnrs url)
  (make-directory* cache-dir)
  (let* ((cache-basename (string-append rnrs ".tar.gz"))
         (cache-filename (build-path cache-dir cache-basename)))
    (unless (file-exists? cache-filename)
      (fprintf (current-error-port)
               "Downloading <~a> from <~a>...~%" cache-basename url)
      (call/input-url
       (string->url url)
       get-pure-port
       (lambda (in)
         (call-with-atomic-output-file
          cache-filename
          (lambda (out _) (write-bytes (port->bytes in) out))))))
    cache-filename))

(define (download-and-extract rnrs url)
  (let ((files '()))
    (call-with-input-file (download rnrs url)
      (lambda (tgz-input)
        (delete-tmp-files tmp-dir)
        (make-directory* tmp-dir)
        (make-directory* rnrs)
        (untgz
         tgz-input
         #:dest tmp-dir
         #:filter
         (lambda (_file-path file-path file-type . _)
           (cond ((and (equal? 'file file-type)
                       (equal? #".tex" (path-get-extension file-path)))
                  (set! files (cons file-path files)))
                 (else #f))))))
    (for-each (lambda (file-path)
                (rename-file-or-directory
                 file-path
                 (build-path rnrs (file-name-from-path file-path))))
              (sort files path<?))
    (delete-tmp-files tmp-dir)))

(download-and-extract
 "r4rs"
 "https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r4rs.tar.gz")

(download-and-extract
 "r5rs"
 "https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs.tar.gz")

(download-and-extract
 "r6rs"
 "http://www.r6rs.org/final/r6rs.tar.gz")

(download-and-extract
 "r7rs"
 "https://bitbucket.org/cowan/r7rs/get/draft-10.tar.gz")
