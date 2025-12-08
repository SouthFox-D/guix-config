(define-module (arch web)
  #:use-module (arch services)
  #:use-module (arch shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (foreign-nginx-service-type
            default-proxy-service))

(define (emit-nginx-location-config config)
  (let ((uri (nginx-location-configuration-uri config))
        (body (nginx-location-configuration-body config)))
    (list
     "      location " uri " {\n"
     (map (lambda (x) (list "        " x "\n")) body)
     "      }\n")))

(define* (emit-nginx-server-config server #:optional (context 'http))
  (define (config-domain-strings names)
    "Return a string denoting the nginx config representation of NAMES, a list
of domain names."
    (map (match-lambda
           ('default "_ ")
           ((? string? str) (list str " ")))
         names))
  (define (config-index-strings names)
    "Return a string denoting the nginx config representation of NAMES, a list
of index files."
    (map (match-lambda
           ((? string? str) (list str " ")))
         names))
  (let ((listen (nginx-server-configuration-listen server))
        (server-name (nginx-server-configuration-server-name server))
        (ssl-certificate (nginx-server-configuration-ssl-certificate server))
        (ssl-certificate-key
         (nginx-server-configuration-ssl-certificate-key server))
        (root (nginx-server-configuration-root server))
        (index (nginx-server-configuration-index server))
        (locations (nginx-server-configuration-locations server))
        (raw-content (nginx-server-configuration-raw-content server)))
    (define-syntax-parameter <> (syntax-rules ()))
    (define-syntax-rule (and/l x tail ...)
      (let ((x* x))
        (if x*
            (syntax-parameterize ((<> (identifier-syntax x*)))
              (list tail ...))
            '())))
    (list
     "    server {\n"
     (map (lambda (directive) (list "      listen " directive ";\n")) listen)
     "      server_name " (config-domain-strings server-name) ";\n"
     (and/l ssl-certificate     "      ssl_certificate " <> ";\n")
     (and/l ssl-certificate-key "      ssl_certificate_key " <> ";\n")
     (if (and (eq? context 'http)
              (not (equal? "" root)))
         (list "      root " root ";\n")
         "")
     (if (and (eq? context 'http)
              (not (null? index)))
         (list "      index " (config-index-strings index) ";\n")
         "")
     "\n"
     (map emit-nginx-location-config locations)
     "\n"
     (map (lambda (x) (list "      " x "\n")) raw-content)
     "    }\n")))

(define (default-nginx-config config)
  (define (flatten . lst)
    "Return a list that recursively concatenates all sub-lists of LST."
    (define (flatten1 head out)
      (if (list? head)
          (fold-right flatten1 out head)
          (cons head out)))
    (fold-right flatten1 '() lst))
  (define (nginx-access-log-file config)
    (string-append (nginx-configuration-log-directory config)
                   "/access.log"))
  (define (nginx-error-log-file config)
    (string-append (nginx-configuration-log-directory config)
                   "/error.log"))
  (define emit-global-directive
    (match-lambda
      ((key . (? list? alist))
       (format #f "~a { ~{~a~}}~%" key (map emit-global-directive alist)))
      ((key . value)
       (format #f "~a ~a;~%" key value))))
  (define emit-nginx-log-format-config
    (match-lambda
      (($ <nginx-log-format-configuration> name escape format)
       (list "    log_format " (symbol->string name) " escape="
             (symbol->string escape) " " format ";\n"))))
  (let ((nginx (nginx-configuration-nginx config))
        (global-directives (nginx-configuration-global-directives config))
        (log-directory (nginx-configuration-log-directory config))
        (run-directory (nginx-configuration-run-directory config))
        (log-level (nginx-configuration-log-level config))
        (log-format (nginx-configuration-log-format config))
        (log-formats (nginx-configuration-log-formats config))
        (server-blocks (nginx-configuration-server-blocks config))
        (server-names-hash-bucket-size (nginx-configuration-server-names-hash-bucket-size config))
        (server-names-hash-bucket-max-size (nginx-configuration-server-names-hash-bucket-max-size config))) 
    (apply mixed-text-file "nginx.conf"
           (flatten
            "user www-data;\n"
            "pid " run-directory "/pid;\n"
            "error_log " (nginx-error-log-file config) " " (symbol->string log-level) ";\n"
            (map emit-global-directive global-directives)
            "http {\n"
            "    client_body_temp_path " run-directory "/client_body_temp;\n"
            "    proxy_temp_path " run-directory "/proxy_temp;\n"
            "    fastcgi_temp_path " run-directory "/fastcgi_temp;\n"
            "    uwsgi_temp_path " run-directory "/uwsgi_temp;\n"
            "    scgi_temp_path " run-directory "/scgi_temp;\n"
            (map emit-nginx-log-format-config log-formats)
            "    access_log " (nginx-access-log-file config) " " (symbol->string log-format) ";\n"
            "    include " nginx "/share/nginx/conf/mime.types;\n"
            (if server-names-hash-bucket-size
                (string-append
                 "    server_names_hash_bucket_size "
                 (number->string server-names-hash-bucket-size)
                 ";\n")
                "")
            (if server-names-hash-bucket-max-size
                (string-append
                 "    server_names_hash_bucket_max_size "
                 (number->string server-names-hash-bucket-max-size)
                 ";\n")
                "")
            "\n"
            (map emit-nginx-server-config server-blocks)
            "}\n"))))

(define (nginx-activation config)
  (let ((run-directory (nginx-configuration-run-directory config))
        (log-directory (nginx-configuration-log-directory config))
        (file (nginx-configuration-file config)))
   #~(begin
       (use-modules (guix build utils))

       (format #t "creating nginx log directory '~a'~%" #$log-directory)
       (mkdir-p #$log-directory)
       (format #t "creating nginx run directory '~a'~%" #$run-directory)
       (mkdir-p #$run-directory)
       (format #t "creating nginx temp directories '~a/{client_body,proxy,fastcgi,uwsgi,scgi}_temp'~%" #$run-directory)
       (mkdir-p (string-append #$run-directory "/client_body_temp"))
       (mkdir-p (string-append #$run-directory "/proxy_temp"))
       (mkdir-p (string-append #$run-directory "/fastcgi_temp"))
       (mkdir-p (string-append #$run-directory "/uwsgi_temp"))
       (mkdir-p (string-append #$run-directory "/scgi_temp")))))

(define foreign-nginx-service-type
  (service-type
   (inherit
    (system->foreign-service-type nginx-service-type))
   (extensions
    (list
     (service-extension arch-activation-service-type
                        nginx-activation)
     (service-extension arch-shepherd-service-type
                        (lambda (config)
                          (let* ((file (nginx-configuration-file config))
                                (nginx (nginx-configuration-nginx config))
                                (run-directory (nginx-configuration-run-directory config))
                                (pid-file (in-vicinity run-directory "pid")))
                            (list (shepherd-service
                                   (documentation "Start nginx")
                                   (provision '(nginx))
                                   (start #~(lambda _
                                             (invoke #$(file-append nginx "/sbin/nginx")
                                                     "-c" #$(or file (default-nginx-config config)))
                                             (read-pid-file #$pid-file)))
                                   (stop #~(make-kill-destructor))
                                   (auto-start? #t))))))))))

(define (default-proxy-service server-name-list server-port)
  (nginx-server-configuration
   (listen '("80"))
   (server-name server-name-list)
   (root "")
   (index '())
   (locations
    (list
     (nginx-location-configuration
      (uri "/")
      (body `(,(string-append "proxy_pass http://127.0.0.1:" server-port ";")
              "proxy_set_header Host $host;"
              "proxy_set_header X-Real-IP $remote_addr;"
              "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
              "proxy_set_header X-Forwarded-Proto $scheme;")))))))
