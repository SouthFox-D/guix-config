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
             (gnu home services mcron)
             (fox packages)
             (fox services))

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
                         "fzf"
                         "mu"
                         ))
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
                  (".config/zellij/config.kdl" ,(local-file "files/zellij.kdl"))

                  ;; bin
                  (".local/bin/chill-player" ,(local-file "files/bin/chill-player.hy" #:recursive? #t))
                  (".local/bin/flk" ,(local-file "files/bin/flk.sh" #:recursive? #t))

                  ;; mail
                  (".config/offlineimap/config" ,(local-file "files/mail/offlineimaprc"))

                  ;; fcitx5
                  (".config/fcitx5/config" ,(local-file "files/fcitx5/config"))
                  (".config/fcitx5/profile" ,(local-file "files/fcitx5/profile"))
                  (".config/fcitx5/conf/keyboard.conf" ,(local-file "files/fcitx5/conf/keyboard.conf"))
                  (".config/fcitx5/conf/notifications.conf" ,(local-file "files/fcitx5/conf/notifications.conf"))
                  ))
       (service home-zsh-service-type
                (home-zsh-configuration
                 (zshrc (list (local-file "files/zshrc")))))
       (service home-mcron-service-type
                (home-mcron-configuration
                 (jobs (list #~(job '(next-minute
                                      (range 0 60 20))
                                    (lambda ()
                                      (system* "offlineimap")))
                             ))))
       ))))
