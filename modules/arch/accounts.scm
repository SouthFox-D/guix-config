(define-module (arch accounts)
  #:use-module (arch services)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (arch-account-configuration
            arch-account-deploy-service-type))

(define-record-type* <arch-account-configuration>
  arch-account-configuration make-arch-account-configuration
  arch-account-configuration?
  (name arch-account-configuration-name)
  (shell arch-account-configuration-shell))

(define (compute-account-entry config)
  #~(begin
      (use-modules (gnu build accounts)
                   (guix build utils)
                   (ice-9 rdelim)
                   (ice-9 textual-ports)
                   (srfi srfi-1))

      (define backup-directory
        (string-append "/root" "/" (number->string (current-time))
                       "-arch-etc-backup"))

      (mkdir-p backup-directory)
      (display "Backing up /etc/passwd\n")
      (copy-file "/etc/passwd" (string-append backup-directory "/passwd"))
      (display "Backing up /etc/shells\n")
      (copy-file "/etc/shells" (string-append backup-directory "/shells"))

      (define current-passwd (read-passwd))
      (define current-shell (string-split
                             (call-with-input-file
                                 "/etc/shells"
                               get-string-all) #\newline))

      (define (set-account name shell)
        (map (lambda (ac)
               (if (equal? (password-entry-name ac) name)
                   (password-entry (inherit ac) (shell shell))
                   ac))
             current-passwd))

      (define (set-shell shell)
        (map (lambda (as)
               (if (equal?
                    (last-pair (string-split as #\/))
                    (last-pair (string-split shell #\/)))
                   shell
                   as))
             current-shell))

      (define arch-accounts
        '#$(map (lambda (c)
                  (list (arch-account-configuration-name c)
                        (file-append (specification->package
                                      (arch-account-configuration-shell c))
                                     "/bin/" (arch-account-configuration-shell c)))) config))
      (define account-shell (delete-duplicates (map cadr arch-accounts) equal?))

      (map (lambda (account) (set! current-passwd (apply set-account account))) arch-accounts)
      (map (lambda (shell) (set! current-shell (set-shell shell))) account-shell)

      (write-passwd current-passwd)
      (call-with-output-file "/etc/shells"
        (lambda (port)
          (put-string port (string-join current-shell "\n"))))))

(define arch-account-deploy-service-type
  (service-type (name 'arch-account-deploy)
                (extensions
                 (list
                  (service-extension
                   arch-sync-service-type
                   compute-account-entry)
                  (service-extension
                   arch-profile-service-type
                   (lambda (configs)
                     (map (lambda (config)
                            (specification->package
                             (arch-account-configuration-shell config)))
                          configs)))))
                (compose identity)
                (extend append)
                (description "Deploy arch account")))
