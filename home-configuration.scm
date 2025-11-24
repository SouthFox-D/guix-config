;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages base)
             (gnu packages bittorrent)
             (gnu packages syncthing)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (fox packages)
             (fox wow-addons)
             (fox services))

(load "common.scm")

(home-environment
 (packages (append (specifications->packages
                    (append
                     (list
                      "glibc-locales"
                      "zsh"
                      "zsh-completions"
                      "zsh-autosuggestions"
                      "zsh-syntax-highlighting"
                      "fzf"
                      "guile-next"
                      "guile-hoot"
                      "guile-goblins"
                      )
                     (if (and touchable-machine?
                              (not deck-machine?)
                              (not pi-machine?))
                         '("mu"
                           "anki-bin"
                           "nyxt-bin"
                           "v2rayn-bin"
                           "yay-bin"
                           "rimerc-zrm"
                           "haunt"
                           )
                         '())
                     (if pi-machine?
                         '("aria2")
                         '())))
                   (list
                    zellij-bin)
                   (if (and (not work-machine?)
                            (not deck-machine?)
                            (not pi-machine?))
                         (list opentabletdriver-bin)
                         '())
                   ))
 (services
  (append
   (list
    (service oh-my-zsh-service-type)
    (service home-zsh-service-type
             (home-zsh-configuration
              (zshrc (list (eval-file "files/zshrc")))))
    (service home-files-service-type
             (append
              `((".config/zellij/config.kdl" ,(eval-file "files/zellij.kdl"))
                (".local/bin/flk" ,(local-file "files/bin/flk.sh" #:recursive? #t)))
              (if (and touchable-machine?
                       (not deck-machine?)
                       (not pi-machine?))
                  `((".config/hypr/hyprland.conf" ,(eval-file "files/hyprland.conf"))
                    (".config/hypr/hyprlock.conf" ,(eval-file "files/hyprlock.conf"))
                    (".config/hypr/hyprpaper.conf" ,(local-file "files/hyprpaper.conf"))
                    (".config/nyxt/config.lisp" ,(local-file "files/nyxt.lisp"))
                    (".config/kitty/kitty.conf" ,(local-file "files/kitty.conf"))

                    (".local/bin/chill-player" ,(local-file "files/bin/chill-player.hy" #:recursive? #t))
                    (".local/bin/update-wallpaper" ,(local-file "files/bin/update-wallpaper.hy" #:recursive? #t))
                    (".local/bin/literature-clock" ,(local-file "files/bin/literature-clock.hy" #:recursive? #t))
                    ;; mail
                    (".config/offlineimap/config" ,(local-file "files/mail/offlineimaprc"))
                    ;; fcitx5
                    (".config/fcitx5/config" ,(local-file "files/fcitx5/config"))
                    (".config/fcitx5/profile" ,(local-file "files/fcitx5/profile"))
                    (".config/fcitx5/conf/keyboard.conf" ,(local-file "files/fcitx5/conf/keyboard.conf"))
                    (".config/fcitx5/conf/notifications.conf" ,(local-file "files/fcitx5/conf/notifications.conf"))
                    ;; anki
                    (".local/share/Anki2/addons21/dummp_tts/__init__.py" ,(local-file "files/anki/dummy_tts.py"))
                    ;; waybar
                    (".config/waybar/config.jsonc" ,(local-file "files/waybar/config.jsonc"))
                    (".config/waybar/style.css" ,(local-file "files/waybar/style.css"))
                    )
                  (if deck-machine?
                      `(("BN/World of Warcraft/_retail_/Interface/AddOns"
                         ,(directory-union "AddOns" (list
                                                     cell
                                                     consoleport
                                                     deadly-boss-mods
                                                     immersion
                                                     weakauras))))
                      '())))))
   (if (and touchable-machine?
            (not deck-machine?)
            (not pi-machine?))
       (append
        (list (simple-service
               'rime-config-deploy
               home-activation-service-type
               #~(begin
                   (system* "mkdir" "-p" #$(string-append (getenv "HOME") "/.local/share/fcitx5"))
                   (system* "cp" "-r"
                            #$(string-append (getenv "HOME") "/.guix-home/profile/share/fctix5/rime")
                            #$(string-append (getenv "HOME") "/.local/share/fcitx5/"))))))
       '())
   (if pi-machine?
       (append
        (list (simple-service 'pi-home-shepherd home-shepherd-service-type
                              (list
                               (shepherd-service
                                (documentation "Start syncthing")
                                (provision '(syncthing))
                                (start #~(make-forkexec-constructor
                                          (list #$(file-append syncthing "/bin/syncthing")
                                                "serve" "--no-browser" "--no-restart")))
                                (stop #~(make-kill-destructor))
                                (auto-start? #t))
                               (shepherd-service
                                (documentation "Start aria2")
                                (provision '(aria2))
                                (start #~(make-forkexec-constructor
                                          (list #$(file-append aria2 "/bin/aria2c")
                                                (string-append "--conf-path="
                                                               (getenv "HOME") "/.config/aria2/aria2.conf"))))
                                (stop #~(make-kill-destructor))
                                (auto-start? #t))
                               (shepherd-service
                                (documentation "Start resilio sync")
                                (provision '(rslsync))
                                (start #~(make-forkexec-constructor
                                          (list (string-append (getenv "HOME") "/.config/guix/current/bin/guix")
                                                "shell" "--network" "-C"
                                                "-L" (string-append (getenv "HOME") "/.config/guix/modules")
                                                "resilio-sync-bin"
                                                "--"
                                                "rslsync" "--nodaemon" "--webui.listen" "127.0.0.1:8089")
                                          #:directory (string-append (getenv "HOME") "/resilio-sync")
                                          #:log-file "/dev/null"))
                                (stop #~(make-kill-destructor))
                                (auto-start? #t))
                               (shepherd-service
                                (documentation "Start calibre-web")
                                (provision '(calibre-web))
                                (start #~(make-forkexec-constructor
                                          (list #$(string-append (getenv "HOME") "/.config/guix/current/bin/guix")
                                                "shell" "podman" "podman-compose"
                                                "--"
                                                "podman" "compose" "up" "--force-recreate")
                                          #:directory (string-append (getenv "HOME") "/calibre-web")))
                                (stop #~(make-kill-destructor))
                                (auto-start? #t))))
              (simple-service 'aria2-config-deploy home-activation-service-type
                              #~(begin
                                  (system* "mkdir" "-p" #$(string-append (getenv "HOME") "/.aria2/"))
                                  (map (lambda (path)
                                         (system* "touch" path))
                                       (list #$(string-append (getenv "HOME") "/.aria2/aria2.session")
                                             #$(string-append (getenv "HOME") "/.aria2/dht.session")
                                             #$(string-append (getenv "HOME") "/.aria2/dht6.session")))
                                  (system* "mkdir" "-p" #$(string-append (getenv "HOME") "/.config/aria2/"))
                                  (system* "cp" "-f"
                                           #$(local-file "files/aria2/aria2.conf")
                                           #$(string-append (getenv "HOME") "/.config/aria2/aria2.conf"))))))
       '())
   (if (and touchable-machine?
            (not work-machine?)
            (not deck-machine?)
            (not pi-machine?))
       (append
        (list
         (simple-service 'my-timer home-shepherd-service-type
                         (list (shepherd-timer
                                '(update-mail)
                                #~(calendar-event #:minutes (iota 3 0 20))
                                #~("offlineimap"))))
         (simple-service 'otd-shepherd home-shepherd-service-type
                         (list
                          (shepherd-service
                           (documentation "Start otd")
                           (provision '(otd))
                           (start #~(make-forkexec-constructor
                                     (list #$(file-append opentabletdriver-bin "/bin/otd-daemon"))
                                     #:environment-variables
                                     (append (default-environment-variables)
                                             '("DISPLAY=:1"))))
                           (stop #~(make-kill-destructor))
                           (auto-start? #f))))))
       '())
   )))
