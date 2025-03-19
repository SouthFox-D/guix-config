(define (touchable-machine?)
  (not (or (member (gethostname) (list "mastfox" "basefox"))
           (equal? "lighthouse" (getlogin)))))
