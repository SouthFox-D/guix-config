(use-modules (gnu services)
             (guix gexp)
             (guix store)
             (guix derivations)
             (guix profiles)
             (guix monads)
             (guix modules)
             (guix sets)
             (guix build utils)
             (guix scripts package)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 ftw)
             (srfi srfi-1))


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
   "tmux"
   "iftop"
   ))


(define (arch-derivation entries mextensions)
  "Return as a monadic value the derivation of the 'arch'
directory containing the given entries."
  (mlet %store-monad ((extensions (mapm/accumulate-builds identity
                                                          mextensions)))
        (lower-object
         (file-union "arch" (append entries (concatenate extensions))))))

(define arch-service-type
  ;; This is the ultimate service type, the root of the home service
  ;; DAG.  The service of this type is extended by monadic name/item
  ;; pairs.  These items end up in the "arch-environment directory" as
  ;; returned by 'arch-environment-derivation'.
  (service-type (name 'arch)
                (extensions '())
                (compose identity)
                (extend arch-derivation)
                (default-value '())
                (description
                 "Build the arch environment top-level directory,
which in turn refers to everything the home environment needs: its
packages, configuration files, activation script, and so on.")))

(define (files->files-directory files)
  "Return a @code{files} directory that contains FILES."
  (define (assert-no-duplicates files)
    (let loop ((files files)
               (seen (set)))
      (match files
        (() #t)
        (((file _) rest ...)
         (when (set-contains? seen file)
           (raise (formatted-message (G_ "duplicate '~a' entry for files/")
                                     file)))
         (loop rest (set-insert file seen))))))

  ;; Detect duplicates early instead of letting them through, eventually
  ;; leading to a build failure of "files.drv".
  (assert-no-duplicates files)

  (file-union "files" files))

(define arch-files-directory "files")

(define (files-entry files)
  "Return an entry for the files}
directory containing FILES."
  (with-monad %store-monad
              (return `((,arch-files-directory ,(files->files-directory files))))))

(define arch-files-service-type
  (service-type (name 'arch-files)
                (extensions
                 (list (service-extension arch-service-type
                                          files-entry)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Files that will be put in
files, and further processed during activation.")))

(define (compute-pacman-sync-script _ gexps)
  (program-file
   "sync"
   (with-imported-modules
    (source-module-closure '((ice-9 popen)
                             (ice-9 rdelim)))
    #~(begin
        (use-modules (ice-9 popen)
                     (ice-9 rdelim))

        (define (list-difference l1 l2)
          (cond ((null? l1)
                 '())
                ((not (member (car l1) l2))
                 (cons (car l1) (list-difference (cdr l1) l2)))
                (else
                 (list-difference (cdr l1) l2))))

        (define arch-user-packages (list #$@%arch-base-packages))
        (define (arch-package-update)
          (let ((port (open-output-pipe "pacman -Syu --noconfirm")))
            (if (not (eqv? 0 (status:exit-val (close-pipe port))))
                (error "Something wrong"))))

        (define (arch-install-packages packages-list)
          (if (not (nil? packages-list))
              (let ((port (open-output-pipe (string-append "pacman -S --noconfirm "
                                                           (string-join packages-list " ")))))
                (display port)
                (if (not (eqv? 0 (status:exit-val (close-pipe port))))
                    (error "Something wrong")))
              (display "Nothing to install...!")))

        (define (arch-get-pacman-package-list)
          (let* ((explicitly-packages (string-split (read-string (open-input-pipe "pacman -Qqe")) #\newline))
                 (all-packages (string-split (read-string (open-input-pipe "pacman -Qq")) #\newline)))
            (display "Hint: Run \n")
            (display (string-append
                      "pacman -R "
                      (string-join (list-difference explicitly-packages arch-user-packages) " ") "\n"))
            (display "command to sync packages.\n")

            (list-difference arch-user-packages all-packages)))

        (arch-package-update)
        (arch-install-packages (arch-get-pacman-package-list))
        ))))

(define (pacman-sync-script-entry sync)
  "Return, as a monadic value, an entry for the sync script
in the arch pacman overlay."
  (with-monad %store-monad
              (return `(("sync" ,sync)))))

(define arch-run-on-pacman-sync-service-type
  (service-type (name 'arch-run-on-pacman-sync)
                (extensions
                 (list (service-extension
                        arch-service-type
                        pacman-sync-script-entry)))
                (compose identity)
                (extend compute-pacman-sync-script)
                (default-value #f)
                (description "Run gexps on pacman sync")))

(define arch-drv-raw
  (with-store
   %store
   (run-with-store
    %store
    (service-value
     (fold-services
      (list
       (service arch-service-type)
       (service arch-files-service-type
                (list
                 `(".arch-package" ,(plain-file "arch-package" (string-join %arch-base-packages "\n")))))
       (service arch-run-on-pacman-sync-service-type)
       )
      #:target-type arch-service-type)))))

(define %arch-profile
  (string-append %profile-directory "/arch-profile"))

(let* ((arch-drv (with-store %store (build-derivations %store (list arch-drv-raw))))
       (arch-drv-output (derivation->output-path arch-drv-raw))
       (number (generation-number %arch-profile))
       (generation (generation-file-name
                    %arch-profile (+ 1 number))))
  (pk generation)
  (switch-symlinks generation arch-drv-output)
  (switch-symlinks %arch-profile generation)
  (primitive-load (string-append arch-drv-output "/sync")))

;; (with-store
;;  %store
;;  (delete-matching-generations %store %arch-profile #f))
