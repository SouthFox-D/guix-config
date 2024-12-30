;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages base)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells)
             (fox packages))

(if (equal? "lighthouse" (getlogin))
    (home-environment
     (packages (append (specifications->packages
                        (list
                         "glibc-locales"
                         "zsh"
                         "zsh-completions"
                         "zsh-autosuggestions"
                         "zsh-syntax-highlighting"
                         "fzf"))
                       (list
                        zellij)))
     (services
      (list
       (service oh-my-zsh-service-type)
       (service home-bash-service-type
                (home-bash-configuration
                 (bashrc (list (local-file "files/bashrc")))))
       (service home-zsh-service-type
                (home-zsh-configuration
                 (zshrc (list (local-file "files/zshrc"))))))))

    (home-environment
     (packages (append (specifications->packages
                        (list
                         "guile-hoot"
                         "guile-next"
                         "zsh"
                         "zsh-completions"
                         "zsh-autosuggestions"
                         "zsh-syntax-highlighting"
                         "fzf"))
                       (list
                        zellij)))
     (services
      (list
       (service oh-my-zsh-service-type)
       (service home-emacs-service-type)
       (service home-files-service-type
                `((".config/hypr/hyprland.conf" ,(local-file "files/hyprland.conf"))
                  (".config/hypr/hyprlock.conf" ,(local-file "files/hyprlock.conf"))
                  (".config/hypr/hyprpaper.conf" ,(local-file "files/hyprpaper.conf"))
                  ;; bin
                  (".local/bin" ,(local-file "files/bin" #:recursive? #t))
                  ))
       (service home-zsh-service-type
                (home-zsh-configuration
                 (zshrc (list (local-file "files/zshrc"))))))))
    )
