(use-modules (guix discovery)
             (gnu packages)
             (guix packages)
             (ice-9 popen))

(define fox-packages
   (string-join (map (lambda (p) (package-name p))
                     (fold-packages
                      (lambda (a b)
                        (cons a b))
                      '() (scheme-modules "modules")))
                " "))

(define build-command
  (string-join `("guix" "build" "-L" "modules" ,fox-packages) " "))

(let ((port (open-output-pipe build-command)))
  (if (not (eqv? 0 (status:exit-val (close-pipe port))))
      (error "Something wrong")))
