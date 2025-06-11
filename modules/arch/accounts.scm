(define-module (arch accounts)
  #:use-module (arch services)
  #:use-module (gnu services)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (arch-account-configuration
            arch-account-deploy-service-type))

(define-record-type* <arch-account-configuration>
  arch-account-configuration make-arch-account-configuration
  arch-account-configuration?
  (name arch-account-configuration-name)
  (shell arch-account-configuration-shell
         (default #f)))

(define (compute-account-entry config)
   #~(begin
       (use-modules (gnu build accounts)
                    (guix build utils))

       (define backup-directory
         (string-append "/root" "/" (number->string (current-time))
                        "-arch-etc-backup"))

       (mkdir-p backup-directory)
       (display "Backing up /etc/passwd\n")
       (copy-file "/etc/passwd" (string-append backup-directory "/passwd"))

       (define current-passwd (read-passwd))

       (define (set-account name shell)
         (map (lambda (ac)
                (if (equal? (password-entry-name ac) name)
                    (password-entry (inherit ac) (shell shell))
                    ac))
              current-passwd))

       (map (lambda (account) (set! current-passwd (apply set-account account)))
            '#$(map (lambda (c)
                      (list (arch-account-configuration-name c)
                            (arch-account-configuration-shell c))) config))

       (write-passwd current-passwd)))

(define arch-account-deploy-service-type
  (service-type (name 'arch-account-deploy)
                (extensions
                 (list (service-extension
                        arch-sync-service-type
                        compute-account-entry)))
                (compose identity)
                (extend append)
                (description "Deploy arch account")))
