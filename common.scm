(define touchable-machine?
  (not (or (member (gethostname) (list "mastfox" "basefox" "alifox" "txfox"))
           (equal? "lighthouse" (getlogin)))))
