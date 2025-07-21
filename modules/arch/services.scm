(define-module (arch services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix profiles)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix sets)
  #:use-module (guix build utils)
  #:use-module (guix scripts package)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (arch-service-type
            arch-files-service-type
            arch-profile-service-type
            arch-sync-service-type
            arch-pacman-sync-service-type
            arch-activation-service-type
            )
  #:re-export (service
               service-type
               service-extension))



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

(define (compute-sync-script init-gexp gexps)
  (gexp->script
   "sync"
   #~(begin
       (let* ((arch-path (string-append (getenv "HOME") "/.guix-arch"))
              (new-arch-env (getenv "GUIX_NEW_ARCH"))
              (new-arch (or new-arch-env
                            ;; Absolute path of the directory of the activation
                            ;; file if called interactively.
                            (canonicalize-path (dirname (car (command-line))))))
              (old-arch-env (getenv "GUIX_OLD_ARCH"))
              (old-arch (or old-arch-env
                            (if (file-exists? arch-path)
                                (readlink arch-path)
                                #f))))
         (setenv "GUIX_NEW_ARCH" new-arch)
         (setenv "GUIX_OLD_ARCH" old-arch))

         #$@gexps
         #$init-gexp

         (setenv "GUIX_NEW_HOME" #f)
         (setenv "GUIX_OLD_HOME" #f))))

(define (sync-script-entry m-activation)
  "Return, as a monadic value, an entry for the sync script
in the arch environment directory."
  (mlet %store-monad ((sync m-activation))
    (return `(("sync" ,sync)))))

(define arch-sync-service-type
  (service-type (name 'arch-sync)
                (extensions
                 (list (service-extension
                        arch-service-type
                        sync-script-entry)))
                (compose (lambda (gexp-entries)
                           (define (get-priority-and-script entry)
                             (if (and (pair? entry) (number? (car entry)))
                                 (values (car entry) (cdr entry))
                                 (values 0 entry)))

                           (let* ((parsed-entries (map (lambda (entry)
                                                         (call-with-values
                                                             (lambda () (get-priority-and-script entry))
                                                           (lambda (priority script)
                                                             (list priority script))))
                                                       gexp-entries))
                                  (sorted-parsed-entries (sort parsed-entries
                                                               (lambda (a b)
                                                                 (< (car a) (car b)))))
                                  (sorted-scripts (map cdr sorted-parsed-entries)))
                             (map (compose identity cadr) sorted-parsed-entries))))
                (extend compute-sync-script)
                (default-value #f)
                (description "Run gexps on sync")))

(define (compute-pacman-sync-script packages)
  (with-imported-modules
   (source-module-closure '((ice-9 popen)
                            (ice-9 rdelim)
                            (ice-9 textual-ports)))
   #~(begin
       (use-modules (ice-9 popen)
                    (ice-9 rdelim)
                    (ice-9 textual-ports))

       (define (list-difference l1 l2)
         (cond ((null? l1)
                '())
               ((not (member (car l1) l2))
                (cons (car l1) (list-difference (cdr l1) l2)))
               (else
                (list-difference (cdr l1) l2))))

       (define arch-user-packages '#$packages)

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
       (arch-install-packages (arch-get-pacman-package-list)))))

(define arch-pacman-sync-service-type
  (service-type (name 'arch-run-on-pacman-sync)
                (extensions
                 (list (service-extension
                        arch-sync-service-type
                        compute-pacman-sync-script)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Run gexps on pacman sync")))

(define (compute-activation-script init-gexp gexps)
  (gexp->script
   "activate"
   #~(begin
       #$@gexps)))

(define (activation-script-entry m-activation)
  "Return, as a monadic value, an entry for the activation script
in the home environment directory."
  (mlet %store-monad ((activation m-activation))
    (return `(("activate" ,activation)))))

(define arch-activation-service-type
  (service-type (name 'arch-activation)
                (extensions
                 (list (service-extension
                        arch-service-type
                        activation-script-entry)))
                (compose identity)
                (extend compute-activation-script)
                (default-value #f)
                (description "Run gexps to activate the current
generation of home environment and update the state of the home
directory.  @command{activate} script automatically called during
reconfiguration or generation switching.  This service can be extended
with one gexp, but many times, and all gexps must be idempotent.")))

(define (packages->profile-entry packages)
  "Return a system entry for the profile containing PACKAGES."
  ;; XXX: 'mlet' is needed here for one reason: to get the proper
  ;; '%current-target' and '%current-target-system' bindings when
  ;; 'packages->manifest' is called, and thus when the 'package-inputs'
  ;; etc. procedures are called on PACKAGES.  That way, conditionals in those
  ;; inputs see the "correct" value of these two parameters.  See
  ;; <https://issues.guix.gnu.org/44952>.
  (mlet %store-monad ((_ (current-target-system)))
    (return `(("profile" ,(profile
                           (content (packages->manifest
                                     (map identity
                                     ;;(options->transformation transformations)
                                     (delete-duplicates packages eq?))))))))))

(define arch-profile-service-type
  (service-type (name 'arch-profile)
                (extensions
                 (list (service-extension
                        arch-service-type
                        packages->profile-entry)))
                (compose concatenate)
                (extend append)
                (description
                 "It contains packages and configuration files.")))
