(use-modules (arch overlay)
             (arch accounts)
             (arch services)
             (arch shepherd)
             (fox services)
             (fox packages)
             (fox template)
             (gnu services)
             (guix gexp)
             (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             ;; (guix store)
             ;; (guix profiles)
             ;; (guix scripts package)
             )

(load "common.scm")

(define %arch-base-packages
  (list
   ;; vps2arch import
   "base"
   "linux"
   "lvm2"
   "openssh"
   "reflector"
   "vi"
   "vim"
   "nss"
   "grub"
   "efibootmgr"
   ;; guix deps
   "git"
   "wget"
   "which"
   ;; something something...
   "sudo"
   "hyfetch"
   "htop"
   "tmux"
   "mosh"
   "fuse2"
   "iftop"
   "tar"
   "unzip"
   "hy"
   "python-requests"
   "ripgrep"
   ))

(define %arch-de-things
  (list
   "hyprland"
   "hyprlock"
   "hyprpaper"
   "waybar"
   "libnotify"
   "dunst"
   "kitty"
   "cliphist"
   "grim"
   "slurp"
   "brightnessctl"
   ))

(define %arch-misc
  (list
   "nyxt"

   "mpv"
   "sdcv"
   "offlineimap"
   "fd"
   "poppler"
   "ffmpegthumbnailer"
   "mediainfo"
   "libvips"
   "imagemagick"

   "fcitx5"
   "fcitx5-rime"
   ))

(define arch-packages
  (append
   %arch-base-packages
   (if touchable-machine?
       (append
        %arch-de-things
        %arch-misc
        )
       (list "rclone")
       )))


(define %arch-base-services
  (list
   (simple-service 'base-arch-file
                   arch-files-service-type
                   (list
                    `("usr/bin/bleed-edge" ,(local-file "files/bin/bleed-edge.hy" #:recursive? #t))))
   (simple-service 'base-arch-package
                   arch-pacman-sync-service-type
                   arch-packages)))

(define arch-services
  (cond ((equal? "deck" (getenv "SUDO_USER"))
         (list
          (simple-service 'deck-shepherd-type arch-shepherd-service-type
                          (list
                           (shepherd-service
                            (documentation "Start SSH daemon")
                            (provision '(sshd))
                            (start #~(make-forkexec-constructor
                                      (list "/usr/sbin/sshd" "-D")))
                            (stop #~(make-kill-destructor))
                            (auto-start? #t))
                           (shepherd-service
                            (documentation "Start zerotier")
                            (provision '(zerotier))
                            (start #~(make-forkexec-constructor
                                      (list #$(file-append zerotier "/sbin/zerotier-one"))))
                            (stop #~(make-kill-destructor))
                            (auto-start? #t))
                           (shepherd-service
                            (documentation "Start sing-box daemon")
                            (provision '(sing-box))
                            (start #~(make-forkexec-constructor
                                      (list #$(file-append sing-box-bin "/bin/sing-box")
                                            "run" "-C" "/home/drift-bottle/sing-box/conf")))
                            (stop #~(make-kill-destructor))
                            (auto-start? #t))))))
        (touchable-machine?
         (list
          (service
           arch-account-deploy-service-type
           (list (arch-account-configuration
                  (name (getenv "SUDO_USER") )
                  (shell "zsh"))))))
        ((equal? "mastfox" (gethostname))
         (list
          (service
           fox-template-deploy-service-type
           (list (fox-template-configuration
                  (template `(,(local-file "files/infra/rclone.conf")
                              "/root/.config/rclone/rclone.ini")))))
          (service arch-files-service-type
                   (list
                    `("usr/bin/infra-backup" ,(local-file "utils/backup.hy" #:recursive? #t))))
          (simple-service
           'server-timer
           arch-shepherd-service-type
           (list (shepherd-timer
                  '(backup)
                  #~(calendar-event #:hours '(5) #:minutes '(0))
                  #~(#$(string-append %arch-profile "/files/usr/bin/infra-backup")
                       #$(gethostname)))))))
        ((equal? "alifox" (gethostname))
         (list
          (service
           fox-template-deploy-service-type
           (list (fox-template-configuration
                  (secret '("ANKI_CONNECT_ENDPOINT" "ANKI_CONNECT_KEY")))))
          (service arch-files-service-type
                   (list
                    `("usr/bin/cron-task" ,(local-file "utils/cron_task.hy" #:recursive? #t))))
          (simple-service
           'server-timer
           arch-shepherd-service-type
           (list (shepherd-timer
                  '(sync-anki)
                  #~(calendar-event #:minutes (iota 4 0 15))
                  #~(#$(string-append %arch-profile "/files/usr/bin/cron-task") "sync-anki"))))))
        (else '())))

;; TODO make sure template deploy after pacman sync not rely on append order
(build-arch-drv (append arch-services %arch-base-services))

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
