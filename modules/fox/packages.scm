(define-module (fox packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix build-system copy)
  #:use-module (gnu services)
  #:export (zellij
            oh-my-zsh-service-type
            home-emacs-service-type))

(define-public oh-my-zsh
  (let ((commit "ca5471fe496f00007727fd26db762d19519c2e8f")
        (url "https://github.com/ohmyzsh/ohmyzsh.git"))
    (package
      (name "oh-my-zsh")
      (version "231115")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url url)
                      (commit commit)))
                (sha256
                 (base32
                  "08a9x04jgli4xqclaqzyvrzb74kngnf2vg4q6wyqanrpskgbp3mc"))))
      (build-system copy-build-system)
      (synopsis "Oh My Zsh is an open source, community-driven framework for managing your zsh configuration.")
      (description "🙃A delightful community-driven (with 2,400+ contributors) framework for managing
your zsh configuration. Includes 300+ optional plugins
(rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up
your morning, and an auto-update tool that makes it easy to keep up with the latest updates from the community. ")
      (home-page "https://ohmyz.sh/")
      (license expat))))

(define oh-my-zsh-service-type
  (service-type (name 'oh-my-zsh)
                (description "oh my zsh")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        (lambda (_) (list `(".oh-my-zsh" ,oh-my-zsh))))))
                (default-value '())))

(define-public zellij
  (package
    (name "zellij")
    (version "0.41.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/zellij-org/zellij/releases/download/v"
                    version "/zellij-x86_64-unknown-linux-musl.tar.gz"))
              (sha256
               (base32 "113c9agbx36hiq6a1kf2jydrv3h3cd8s0albnwxi0qd1c0n1rxyw"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:install-plan
      #~'(("zellij" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "-xvf" source))))))
    (home-page "https://github.com/zellij-org/zellij")
    (synopsis "A terminal workspace with batteries included.")
    (description
     "A terminal workspace with batteries included.")
    (license expat)))

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
    (stop #~(make-kill-destructor)))))

(define home-emacs-service-type
  (service-type (name 'emacs-configuration)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-emacs-shepherd-service)))
                (default-value '())
                (description "Configures Emacs and installs packages to home-profile.")))
