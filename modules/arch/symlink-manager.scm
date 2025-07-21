(define-module (arch symlink-manager)
  #:use-module (arch services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (arch-symlink-manager-service-type))



(define (update-symlinks-script)
  (program-file
   "update-symlinks"
   (with-imported-modules (source-module-closure
                           '((guix build utils)
                             (guix i18n)))
     #~(begin
         (use-modules (ice-9 ftw)
                      (ice-9 match)
                      (srfi srfi-1)
                      (guix i18n)
                      (guix build utils))

         (define home-directory
           (getenv "HOME"))

         (define backup-directory
           (string-append home-directory "/" (number->string (current-time))
                          "-arch-legacy-configs-backup"))

         (define (target-file file)
           ;; Return the target of FILE, a config file name sans leading dot
           ;; such as "config/fontconfig/fonts.conf" or "bashrc".
           (string-append "/" file))

         (define (no-follow-file-exists? file)
           "Return #t if file exists, even if it's a dangling symlink."
           (->bool (false-if-exception (lstat file))))

         (define (symlink-to-store? file)
           (catch 'system-error
             (lambda ()
               (store-file-name? (readlink file)))
             (lambda args
               (if (= EINVAL (system-error-errno args))
                   #f
                   (apply throw args)))))

         (define (backup-file file)
           (define backup
             (string-append backup-directory "/" file))

           (mkdir-p backup-directory)
           (format #t (G_ "Backing up ~a...") (target-file file))
           (mkdir-p (dirname backup))
           (copy-file (target-file file) backup)
           (delete-file (target-file file))
           (display (G_ " done\n")))

         (define (cleanup-symlinks home-generation)
           ;; Delete from GUIX-ARCH files that originate in HOME-GENERATION, the
           ;; store item containing a home generation.
           (define config-file-directory
             ;; Note: Trailing slash is needed because "files" is a symlink.
             (string-append home-generation "/" "files" "/"))

           (define (strip file)
             (string-drop file
                          (+ 1 (string-length config-file-directory))))

           (format #t (G_ "Cleaning up symlinks from previous arch at ~a.~%")
                   home-generation)
           (newline)

           (file-system-fold
            (const #t)
            (lambda (file stat _)                 ;leaf
              (let ((file (target-file (strip file))))
                (when (no-follow-file-exists? file)
                  ;; DO NOT remove the file if it is no longer a symlink to
                  ;; the store, it will be backed up later during
                  ;; create-symlinks phase.
                  (if (symlink-to-store? file)
                      (begin
                        (format #t (G_ "Removing ~a...") file)
                        (delete-file file)
                        (display (G_ " done\n")))
                      (format
                       #t
                       (G_ "Skipping ~a (not a symlink to store)... done\n")
                       file)))))

            (const #t)                            ;down
            (lambda (directory stat _)            ;up
              (unless (string=? directory config-file-directory)
                (let ((directory (target-file (strip directory))))
                  (catch 'system-error
                    (lambda ()
                      (rmdir directory)
                      (format #t (G_ "Removed ~a.\n") directory))
                    (lambda args
                      (let ((errno (system-error-errno args)))
                        (cond
                         ((= ENOTEMPTY errno)
                          (format
                           #t
                           (G_ "Skipping ~a (not an empty directory)... done\n")
                           directory))
                         ;; This happens when the directory is a mounted device.
                         ((= EBUSY errno)
                          (format
                           #t
                           (G_ "Skipping ~a (underlying device is busy)... done\n")
                           directory))
                         ((= ENOENT errno) #t)
                         ((= ENOTDIR errno) #t)
                         (else
                          (apply throw args)))))))))
            (const #t)                            ;skip
            (const #t)                            ;error
            #t                                    ;init
            config-file-directory
            lstat)

           (display (G_ "Cleanup finished.\n\n")))

         (define (create-symlinks home-generation)
           ;; Create in $HOME symlinks for the files in HOME-GENERATION.
           (define config-file-directory
             ;; Note: Trailing slash is needed because "files" is a symlink.
             (string-append home-generation "/" "files" "/"))

           (define (strip file)
             (string-drop file
                          (+ 1 (string-length config-file-directory))))

           (define (source-file file)
             (readlink (string-append config-file-directory file)))

           (file-system-fold
            (const #t)                            ;enter?
            (lambda (file stat result)            ;leaf
              (let ((source (source-file (strip file)))
                    (target (target-file (strip file))))
                (when (no-follow-file-exists? target)
                  (backup-file (strip file)))
                (format #t (G_ "Symlinking ~a -> ~a...")
                        target source)
                (symlink source target)
                (display (G_ " done\n"))))
            (lambda (directory stat result)       ;down
              (unless (string=? directory config-file-directory)
                (let ((target (target-file (strip directory))))
                  (when (and (no-follow-file-exists? target)
                             (not (file-is-directory? target)))
                    (backup-file (strip directory)))

                  (catch 'system-error
                    (lambda ()
                      (mkdir target))
                    (lambda args
                      (let ((errno (system-error-errno args)))
                        (unless (= EEXIST errno)
                          (format #t (G_ "failed to create directory ~a: ~s~%")
                                  target (strerror errno))
                          (apply throw args))))))))
            (const #t)                            ;up
            (const #t)                            ;skip
            (const #t)                            ;error
            #t                                    ;init
            config-file-directory))

         (let* ((arch     (string-append home-directory "/.guix-arch"))
                (pivot    (string-append arch ".new"))
                (new-arch (getenv "GUIX_NEW_ARCH"))
                (old-arch (getenv "GUIX_OLD_ARCH")))
           (when old-arch
             (cleanup-symlinks old-arch))

           (create-symlinks new-arch)

           (symlink new-arch pivot)
           (rename-file pivot arch)

           (display (G_" done\nFinished updating symlinks.\n\n")))))))

(define (update-symlinks-gexp _)
  `(100 . ,#~(primitive-load #$(update-symlinks-script))))

(define arch-symlink-manager-service-type
  (service-type (name 'arch-symlink-manager)
                (extensions
                 (list
                  (service-extension
                   arch-sync-service-type
                   update-symlinks-gexp)))
                (default-value #f)
                (description "Provide an @code{update-symlinks}
script, which creates symlinks to configuration files and directories
on every activation.  If an existing file would be overwritten by a
symlink, backs up that file first.")))
