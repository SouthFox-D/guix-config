(use-modules (fox arch-overlay)
             (fox arch-shepherd)
             (fox services)
             (fox packages)
             (gnu services)
             (guix gexp)
             (guix records)
             (guix modules)
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

(define-record-type* <arch-template-configuration>
  arch-template-configuration make-arch-template-configuration
  arch-template-configuration?
  (secret arch-template-configuration-secret
          (default '()))
  (template arch-template-configuration-template
            (default '())))

(define (compute-template-entry config)
  (with-imported-modules
   (source-module-closure '((ice-9 popen)
                            (ice-9 rdelim)
                            (ice-9 textual-ports)
                            (fox services)))
   #~(begin
       (use-modules (ice-9 popen)
                    (ice-9 rdelim)
                    (ice-9 textual-ports)
                    (fox services))

       (define (get-env env-key)
         (let* ((port (open-input-pipe
                       (string-append (getenv "GUIX_ARCH_DRV") "/files/cf-ky.hy" " get " env-key)))
                (str (read-line port)))
           (when (not (eqv? 0 (status:exit-val (close-pipe port))))
             (error (string-append "error get env " env-key)))
           (format #t "Get ~a done\n" env-key)
           str))

       (define (secret-put env-key-list)
         (let ((secret-path "/root/.config/secret.ini"))
           (when (file-exists? secret-path)
             (delete-file secret-path))
           (call-with-output-file secret-path
             (lambda (port)
               (format port "[SECRET]")
               (newline port)
               (for-each (lambda (env-key)
                           (format port "~a=~a" env-key
                                   (get-env env-key))
                           (newline port))
                         env-key-list)))))

       (define (template-put template-file store-path)
         (let ((result-string (eval-template-file template-file)))
           (unless (file-exists? (dirname template-file))
             (mkdir (dirname template-file)))
           (if (file-exists? store-path)
               (rename-file store-path (string-append store-path ".bak")))
           (call-with-output-file store-path
             (lambda (port)
               (put-string port result-string)))))

       (define secret-list '#$(filter (lambda (s) (string? s))
                                      (apply append (map (lambda (c)
                                                           (arch-template-configuration-secret c)) config))))
       (define file-list '#$(filter (lambda (f) (pair? f))
                                     (map (lambda (c)
                                            (if (pair? (arch-template-configuration-template c))
                                                (cons (car (arch-template-configuration-template c))
                                                      (cdr (arch-template-configuration-template c)))
                                                '()))
                                          config)))

       (secret-put secret-list)
       (for-each
        (lambda (f)
          (template-put (car f) (cadr f)))
        file-list))))

(define arch-template-deploy-service-type
  (service-type (name 'arch-template-deploy)
                (extensions
                 (list (service-extension
                        arch-sync-service-type
                        compute-template-entry)
                       (service-extension
                        arch-files-service-type
                        (lambda (_)
                          (list `("cf-ky.hy"
                                  ,(local-file "utils/cf-kv.hy" #:recursive? #t)))))))
                (compose identity)
                (extend append)
                (description "Run gexps on sync")))

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
          (service arch-pacman-sync-service-type
                   (list arch-packages))))
        ((equal? "mastfox" (gethostname))
         ;; TODO make sure template deploy after pacman sync not rely on append order
         (list
          (service
           arch-template-deploy-service-type
           (list (arch-template-configuration
                  (template `(,(local-file "files/infra/rclone.conf")
                              "/root/.config/rclone/rclone.ini")))))
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
                  #~(#$(string-append %arch-profile "/files/backup.hy")
                       #$(gethostname)))))))
        ((equal? "alifox" (gethostname))
         (list
          (service
           arch-template-deploy-service-type
           (list (arch-template-configuration
                  (secret '("ANKI_CONNECT_ENDPOINT" "ANKI_CONNECT_KEY")))))
          (service arch-files-service-type
                   (list
                    `("cron_task.hy" ,(local-file "utils/cron_task.hy" #:recursive? #t))))
          (service arch-pacman-sync-service-type
                   (list arch-packages))
          (simple-service
           'server-timer
           arch-shepherd-service-type
           (list (shepherd-timer
                  '(sync-anki)
                  #~(calendar-event #:minutes (iota 4 0 15))
                  #~(#$(string-append %arch-profile "/files/cron_task.hy") "sync-anki"))))))
        ))

(build-arch-drv arch-services)

;; (define %arch-profile
;;   (string-append %profile-directory "/arch-profile"))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
