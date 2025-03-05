(define-configuration web-buffer
    ((default-modes
      (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

(define-configuration buffer
    ((override-map
      (let ((map (make-keymap "override-map")))
        (define-key map "M-x" 'execute-command)
        (define-key map "C-d" 'scroll-page-down)
        (define-key map "C-u" 'scroll-page-up)
        ))))
