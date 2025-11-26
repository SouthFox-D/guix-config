(define-module (fox packages licenses)
  #:use-module (guix licenses)
  #:export (nonfree custom))

(define license (@@ (guix licenses) license))

(define* (nonfree uri #:optional (comment ""))
  "Return a nonfree license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Nonfree"
           uri
           (string-append
            "This a nonfree license.  Check the URI for details.  "
            comment)))

(define* (custom uri #:optional (comment ""))
  "Return a custom license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Custom"
           uri
           (string-append
            "This a Custom license.  Check the URI for details."
            comment)))
