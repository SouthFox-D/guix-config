(define touchable-machine?
  (not (or (member (gethostname) (list "mastfox" "basefox" "alifox" "txfox"))
           (equal? "lighthouse" (getlogin)))))

(define work-machine?
  (equal? "n100" (gethostname)))
