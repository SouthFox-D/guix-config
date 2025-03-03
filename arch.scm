(use-modules (fox arch-overlay)
             (gnu services)
             (guix gexp)
             ;; (guix store)
             ;; (guix profiles)
             ;; (guix scripts package)
             )

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
   ))

(define %arch-de-things
  (list
   "hyprland"
   "hyprlock"
   "hyprpaper"
   "waybar"
   "libnotify"
   "dunst"
   "wofi"
   "kitty"
   "cliphist"
   "grim"
   "slurp"
   ))

(define %arch-misc
  (list
   "hy"
   "python-requests"

   "mpv"
   "sdcv"
   "offlineimap"
   "fd"
   "ripgrep"
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
   (if (not (or (member (gethostname) '("mastfox" "basefox"))
                (equal? "lighthouse" (getlogin))))
       (append
        %arch-de-things
        %arch-misc
        )
       '())))

(define arch-services
  (list
   (service arch-files-service-type
            (list
             `(".arch-packages" ,(plain-file "arch-package" (string-join arch-packages "\n")))))))

(build-arch-drv arch-services)

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
