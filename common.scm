(define touchable-machine?
  (not (or (member (gethostname) (list "mastfox" "basefox" "alifox" "txfox"))
           (equal? "lighthouse" (getlogin)))))

(define work-machine?
  (equal? "n100" (gethostname)))

(define deck-machine?
  (or (equal? "deck" (getenv "SUDO_USER"))
      (equal? "deck" (getlogin))))
