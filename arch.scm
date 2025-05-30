(use-modules (fox arch-overlay)
             (fox arch-shepherd)
             (fox services)
             (fox packages)
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
       (list "rclone")
       )))

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
                                                  "run" "-C" "/etc/sing-box/conf")))
                                  (stop #~(make-kill-destructor))
                                  (auto-start? #t))))))
        (touchable-machine?
         (list
          (service arch-pacman-sync-service-type
                   (list arch-packages))))
        ((not touchable-machine?)
         (list
          (service arch-files-service-type
                   (list
                    `("backup.hy" ,(local-file "utils/backup.hy" #:recursive? #t))))
          (service arch-pacman-sync-service-type
                   (list arch-packages))
          (simple-service
           'server-timer
           arch-shepherd-service-type
           (list (shepherd-timer
                  '(backup)
                  #~(calendar-event #:hours '(5) #:minutes '(0))
                  #~((string-append #$%arch-profile "/files/backup.hy")
                     #$(gethostname)))))))))

(build-arch-drv arch-services)

(define (get-env env-key)
  (let* ((port (open-input-pipe
                (string-append "utils/cf-kv.hy get " env-key)))
         (str (read-line port)))
    (close-pipe port)
    (when (eof-object? str)
      (error "Failed to get env" env-key))
    (format #t "Get ~a done\n" env-key)
    str))

(define (template-put template-file store-path)
  (let ((result-string (eval-template-file template-file)))
    (unless (file-exists? (dirname "/root/.config/rclone/rclone.conf"))
      (mkdir (dirname "/root/.config/rclone/rclone.conf")))
    (if (file-exists? store-path)
        (rename-file store-path (string-append store-path ".bak")))
    (call-with-output-file store-path
      (lambda (port)
        (put-string port result-string)))))

(when (not touchable-machine?)
  (template-put "files/infra/rclone.conf" "/root/.config/rclone/rclone.conf"))

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
