(use-modules (guix discovery)
             (gnu packages)
             (guix packages)
             (ice-9 popen))

(define fox-packages
  (pk
   (map (lambda (p) (package-name p))
        (fold-packages
         (lambda (a b)
           (cons a b))
         '() (scheme-modules "modules")))))

(define (build-command package)
  (pk (string-join `("guix" "build" "-L" "modules" ,package) " ")))

(map (lambda (package)
       (let ((port (open-output-pipe (build-command package))))
         (if (not (eqv? 0 (status:exit-val (close-pipe port))))
             (error "Something wrong"))))
     fox-packages)
