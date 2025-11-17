(define touchable-machine?
  (not (or (member (gethostname) (list "mastfox" "basefox" "alifox" "txfox" "trailfox"))
           (equal? "lighthouse" (getlogin)))))

(define den-machine?
  (equal? "den" (gethostname)))

(define work-machine?
  (equal? "hive" (gethostname)))

(define deck-machine?
  (or (equal? "deck" (getenv "SUDO_USER"))
      (equal? "deck" (getlogin))))

(define pi-machine?
  (equal? "pifox" (gethostname)))
