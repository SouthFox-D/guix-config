(define-module (fox services)
  #:use-module (fox packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 eval-string)
  #:export (oh-my-zsh-service-type
            home-emacs-service-type

            eval-template-file
            eval-file
            ))

(define oh-my-zsh-service-type
  (service-type (name 'oh-my-zsh)
                (description "oh my zsh")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        (lambda (_) (list `(".oh-my-zsh" ,oh-my-zsh))))))
                (default-value '())))

(define (home-emacs-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Start Emacs")
    (provision '(emacs))
    (auto-start? #t)
    (start
     #~(make-forkexec-constructor
        (list "/usr/bin/emacs"
              "--fg-daemon")
        #:log-file (format #f "~a/.local/var/log/emacs.log" (getenv "HOME"))))
    (stop
     #~(make-system-destructor
        "/usr/bin/emacsclient --eval '(kill-emacs)'")))))

(define home-emacs-service-type
  (service-type (name 'emacs-configuration)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-emacs-shepherd-service)))
                (default-value '())
                (description "Configures Emacs and installs packages to home-profile.")))

(define (eval-template template-string)
  ;; (display template-string)
  (regexp-substitute/global #f
                            ;; FIXME contain %  string will fail
                            "\\{%([^%]*?)%\\}"
                            template-string
                            'pre
                            (lambda (m)
                              (let ((result (eval-string
                                             (substring (match:substring m)
                                                        2
                                                        (- (string-length (match:substring m)) 2)))))
                                (if (unspecified? result)
                                    ""
                                    result)))
                            'post))

(define (eval-template-file file-path)
  (call-with-input-file file-path
    (lambda (port)
      (eval-template (get-string-all port)))))

(define (eval-file file-path)
  (plain-file (string-replace-substring file-path "/"  "-")
              (eval-template-file file-path)))
