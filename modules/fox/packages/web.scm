(define-module (fox packages web)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages web))

(define-public nginx-fox
  (package
    (inherit nginx)
    (name "nginx-fox")
    (arguments
     (substitute-keyword-arguments
         (package-arguments nginx)
       ((#:configure-flags flags)
        #~(cons* "--with-http_realip_module"
                 #$flags))))))
