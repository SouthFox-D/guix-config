(define-module (arch overlay)
  #:use-module (arch services)
  #:use-module (arch symlink-manager)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix derivations)
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
  #:export (%arch-profile
            build-arch-drv
            ))



(define %base-arch-services
  (list
   (service arch-activation-service-type)
   (service arch-guix-oneshot-service-type)
   (service arch-profile-service-type '())
   (service arch-sync-service-type)
   (service arch-symlink-manager-service-type)
   (service arch-service-type)))

(define %arch-profile
  (string-append %profile-directory "/arch-profile"))

(define (build-arch-drv arch-services)
  (let* ((arch-drv-raw
          (with-store
           %store
           (run-with-store
            %store
            (service-value
             (fold-services
              (instantiate-missing-services
               (append %base-arch-services arch-services))
              #:target-type arch-service-type)))))
         (arch-drv (with-store %store (build-derivations %store (list arch-drv-raw))))
         (arch-drv-output (derivation->output-path arch-drv-raw))
         (number (generation-number %arch-profile))
         (generation (generation-file-name
                      %arch-profile (+ 1 number))))
    (pk generation)
    (setenv "GUIX_NEW_ARCH" arch-drv-output)
    (switch-symlinks generation arch-drv-output)
    (switch-symlinks %arch-profile generation)
    (copy-file (string-append arch-drv-output "/files/guix-arch.service")
               "/etc/systemd/system/guix-arch.service")
    (unless (file-exists? "/etc/systemd/system/guix-daemon.service.wants/guix-arch.service")
      (symlink "/etc/systemd/system/guix-arch.service"
               "/etc/systemd/system/guix-daemon.service.wants/guix-arch.service"))
    (system* "systemctl" "daemon-reload")
    (when (file-exists? (string-append arch-drv-output "/sync"))
      (primitive-load (string-append arch-drv-output "/sync")))
    (primitive-load (string-append arch-drv-output "/activate"))
    (setenv "GUIX_NEW_ARCH" #f)))
