(use-modules (fox arch-overlay)
             (fox arch-shepherd)
             (fox packages)
             (gnu services)
             (guix gexp)
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
   "zsh"
   "tar"
   "unzip"
   "hy"
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
   "python-requests"

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
       '())))

(define arch-services
  (if (equal? "deck" (getenv "SUDO_USER"))
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
                                                  "run" "-C" "/etc/sing-box/conf")))
                                  (stop #~(make-kill-destructor))
                                  (auto-start? #t))
                                 )))
      (list
       (service arch-pacman-sync-service-type
                (list arch-packages)))))

(build-arch-drv arch-services)

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
