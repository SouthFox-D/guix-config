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
   "fuse2"
   "iftop"
   ))

(define arch-services
  (list
   (service arch-files-service-type
            (list
             `(".arch-packages" ,(plain-file "arch-package" (string-join %arch-base-packages "\n")))))))

(build-arch-drv arch-services)

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
