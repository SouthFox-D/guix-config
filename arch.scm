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
   "dust"
   ))

(define %arch-de-things
  (append
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

    "noto-fonts-cjk"
    "ttf-nerd-fonts-symbols"
    "ttf-nerd-fonts-symbols-mono"
    )))

(define %arch-misc
  (list
   "firefox"
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
        %arch-misc)
       (list "rclone"))
   (append
    (if den-machine?
        (list "fprintd")
        '()))
   ))


(define %arch-base-services
  (list
   (simple-service 'base-arch-file
                   arch-files-service-type
                   (list
                    `("usr/bin/bleed-edge" ,(local-file "files/bin/bleed-edge.hy" #:recursive? #t))))
   (simple-service 'base-arch-package
                   arch-pacman-sync-service-type
                   arch-packages)))

(define renew-cert-job
  (shepherd-timer
   '(renew-cert)
   #~(calendar-event #:days-of-month '(1) #:hours '(1) #:minutes '(0))
   (list "certbot" "renew" "--nginx")))

(define otd-files-service
  (simple-service
   'otd-arch-file
   arch-files-service-type
   (list
    `("usr/lib/modprobe.d/99-opentabletdriver.conf"
      ,(file-append opentabletdriver-bin "/usr/lib/modprobe.d/99-opentabletdriver.conf"))
    `("usr/lib/modules-load.d/opentabletdriver.conf"
      ,(file-append opentabletdriver-bin "/usr/lib/modules-load.d/opentabletdriver.conf"))
    `("usr/lib/udev/rules.d/70-opentabletdriver.rules"
      ,(file-append opentabletdriver-bin "/usr/lib/udev/rules.d/70-opentabletdriver.rules"))
    `("usr/share/libinput/30-vendor-opentabletdriver.quirks"
      ,(file-append opentabletdriver-bin "/usr/share/libinput/30-vendor-opentabletdriver.quirks")))))

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
         (append
          (list
           (service
            arch-account-deploy-service-type
            (list (arch-account-configuration
                   (name (getenv "SUDO_USER") )
                   (shell "zsh")))))
           (if (not work-machine?)
               (list otd-files-service)
               '())
           (if den-machine?
               (list
                (simple-service 'den-arch-file
                                arch-files-service-type
                                (list
                                 `("usr/lib64/libfprint-2.so.2.0.0"
                                   ,(file-append libfprint-focaltech "/usr/lib64/libfprint-2.so.2.0.0"))
                                 `("usr/lib64/libfprint-2.so.2"
                                   ,(file-append libfprint-focaltech "/usr/lib64/libfprint-2.so.2"))
                                 `("usr/lib64/libfprint-2.so"
                                   ,(file-append libfprint-focaltech "/usr/lib64/libfprint-2.so")))))
               '())))
        ((equal? "basefox" (gethostname))
         (list
          (simple-service
           'server-timer
           arch-shepherd-service-type
           (list renew-cert-job))))
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
                       #$(gethostname)))
                 renew-cert-job))))
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
(cond ((equal? "basefox" (gethostname))
       (build-arch-drv
        (list
         (simple-service 'pi-shepherd-type arch-shepherd-service-type
                         (list
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
                                            "run" "-C" "/etc/sing-box/conf")))
                            (stop #~(make-kill-destructor))
                            (auto-start? #t)))))))
      (else (build-arch-drv (append arch-services %arch-base-services))))


;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
