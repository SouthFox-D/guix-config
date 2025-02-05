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
   ))

(define %arch-de-things
  (list
   "hyprland"
   "hyprlock"
   "hyprpaper"
   "waybar"
   "dunst"
   "wofi"
   "kitty"
   "cliphist"
   "grim"
   "slurp"
   "pavucontrol"
   ))

(define %arch-misc
  (list
   "hy"
   "python-requests"

   "mpv"
   "sdcv"
   ))

(define arch-packages
  (append
   %arch-base-packages
   (if (not (equal? "lighthouse" (getlogin)))
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
