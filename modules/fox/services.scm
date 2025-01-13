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
  #:export (oh-my-zsh-service-type
            home-emacs-service-type))

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
