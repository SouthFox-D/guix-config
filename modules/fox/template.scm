(define-module (fox template)
  #:use-module (arch services)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu home services)
  #:use-module (guix build-system copy)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:export (fox-template-deploy-service-type
            fox-template-configuration))



(define-record-type* <fox-template-configuration>
  fox-template-configuration make-fox-template-configuration
  fox-template-configuration?
  (secret fox-template-configuration-secret
          (default '()))
  (template fox-template-configuration-template
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
                       (string-append (getenv "GUIX_NEW_ARCH") "/files/usr/bin/cf-ky" " get " env-key)))
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
           (unless (file-exists? (dirname store-path))
             (mkdir (dirname store-path)))
           (if (file-exists? store-path)
               (rename-file store-path (string-append store-path ".bak")))
           (call-with-output-file store-path
             (lambda (port)
               (put-string port result-string)))))

       (define secret-list '#$(filter (lambda (s) (string? s))
                                      (apply append (map (lambda (c)
                                                           (fox-template-configuration-secret c)) config))))
       (define file-list '#$(filter (lambda (f) (pair? f))
                                     (map (lambda (c)
                                            (if (pair? (fox-template-configuration-template c))
                                                (cons (car (fox-template-configuration-template c))
                                                      (cdr (fox-template-configuration-template c)))
                                                '()))
                                          config)))

       (secret-put secret-list)
       (for-each
        (lambda (f)
          (template-put (car f) (cadr f)))
        file-list))))

(define fox-template-deploy-service-type
  (service-type (name 'fox-template-deploy)
                (extensions
                 (list (service-extension
                        arch-sync-service-type
                        compute-template-entry)
                       (service-extension
                        arch-files-service-type
                        (lambda (_)
                          (list `("usr/bin/cf-ky"
                                  ;; TODO Is there a better way?
                                  ,(local-file  "../../utils/cf-kv.hy" #:recursive? #t)))))))
                (compose identity)
                (extend append)
                (description "Run gexps on sync")))
