(define-module (arch shepherd)
  #:use-module (arch overlay)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (arch-shepherd-service-type

            arch-shepherd-configuration
            arch-shepherd-configuration?
            arch-shepherd-configuration-shepherd
            arch-shepherd-configuration-auto-start?
            arch-shepherd-configuration-daemonize?
            arch-shepherd-configuration-silent?
            arch-shepherd-configuration-services

            arch-shepherd-transient-service-type
            arch-shepherd-timer-service-type)
  #:re-export (shepherd-service
               shepherd-service?
               shepherd-service-documentation
               shepherd-service-provision
               shepherd-service-canonical-name
               shepherd-service-requirement
               shepherd-service-one-shot?
               shepherd-service-respawn?
               shepherd-service-start
               shepherd-service-stop
               shepherd-service-auto-start?
               shepherd-service-modules

               shepherd-action
               shepherd-configuration-action
               shepherd-trigger-action
               shepherd-timer))

(define-record-type* <arch-shepherd-configuration>
  arch-shepherd-configuration make-arch-shepherd-configuration
  arch-shepherd-configuration?
  (shepherd arch-shepherd-configuration-shepherd
            (default shepherd-for-home)) ;package
  (auto-start? arch-shepherd-configuration-auto-start?
               (default #t))
  (daemonize? arch-shepherd-configuration-daemonize?
              (default #t))
  (silent? arch-shepherd-configuration-silent?
           (default #t))
  (services arch-shepherd-configuration-services
            (default '())))

(define (arch-shepherd-configuration-file config)
  "Return the shepherd configuration file for SERVICES.  SHEPHERD is used
as shepherd package."
  (let* ((daemonize? (arch-shepherd-configuration-daemonize? config))
         (services (arch-shepherd-configuration-services config))
         (_ (assert-valid-graph services))
         (files (map shepherd-service-file services))
         ;; TODO: Add compilation of services, it can improve start
         ;; time.
         ;; (scm->go (cute scm->go <> shepherd))
         )
    (define config
      #~(begin
          (use-modules (srfi srfi-34)
                       (system repl error-handling))

          (define (make-user-module)
            ;; Copied from (shepherd support), where it's private.
            (let ((m (make-fresh-user-module)))
              (module-use! m (resolve-interface '(shepherd service)))
              m))

          (register-services
           (map (lambda (file)
                  (save-module-excursion
                   (lambda ()
                     (set-current-module (make-user-module))
                     (load file))))
                '#$files))

          #$(and daemonize?
                 #~(perform-service-action root-service 'daemonize))

          (format #t "Starting services...~%")
          (let ((services-to-start
                 '#$(append-map shepherd-service-provision
                                (filter shepherd-service-auto-start?
                                        services))))
            (start-in-the-background services-to-start)

            (redirect-port (open-input-file "/dev/null")
                           (current-input-port)))))

    (scheme-file "shepherd.conf" config)))

(define (launch-shepherd-gexp config)
  (let* ((shepherd (arch-shepherd-configuration-shepherd config))
         (silent? (arch-shepherd-configuration-silent? config)))
    (if (arch-shepherd-configuration-auto-start? config)
        (with-imported-modules '((guix build utils))
                               #~(if (file-exists?
                                      "/var/run/shepherd/socket" )
                                     (system*
                                      #$(file-append shepherd "/bin/herd")
                                      "load" "root"
                                      #$(arch-shepherd-configuration-file config))
                                     (system*
                                      #$(file-append shepherd "/bin/shepherd")
                                      #$@(if silent? '("--silent") '())
                                      "--config"
                                      #$(arch-shepherd-configuration-file config))))
        #~"")))

(define (shepherd-xdg-configuration-files config)
  `(("shepherd/init.scm" ,(arch-shepherd-configuration-file config))))

(define arch-shepherd-service-type
  (service-type (name 'arch-shepherd)
                (extensions
                 (list
                  (service-extension
                   arch-activation-service-type
                   launch-shepherd-gexp)
                  (service-extension
                   arch-profile-service-type
                   (lambda (config)
                     `(,(arch-shepherd-configuration-shepherd config))))))
                (compose concatenate)
                (extend
                 (lambda (config extra-services)
                   (arch-shepherd-configuration
                    (inherit config)
                    (services
                     (append (arch-shepherd-configuration-services config)
                             extra-services)))))
                (default-value (arch-shepherd-configuration))
                (description "Configure and install arch Shepherd.")))
