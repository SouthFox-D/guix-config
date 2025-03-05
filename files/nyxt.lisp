(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration buffer
    ((override-map
      (let ((map (make-keymap "override-map")))
        (define-key map "M-x" 'execute-command)))))
