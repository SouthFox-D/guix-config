(define-module (fox packages python-zyx)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages nss)
  #:use-module (guix build-system copy))

(define-public anki-bin
  (package
   (name "anki-bin")
   (version "25.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://files.pythonhosted.org/packages/py3/a/aqt/aqt-"
                         version
                         "-py3-none-any.whl"))
     (sha256
      (base32 "0sgxicm4kqpvqa5ahzh51gzyr7hz246v8ymv8xziwbfnsspcfdls"))))
   (inputs
    `(("anki"
       ,(origin
         (method url-fetch)
         (uri (string-append "https://files.pythonhosted.org/packages/cp39/a/anki/anki-"
                             version
                             "-cp39-abi3-manylinux_2_36_x86_64.whl"))
         (sha256
          (base32 "09ib48sbfchyymspwhkzkwqi5f4s7847v72vjmbmxa1n1q9r9pwd"))))))
   (build-system copy-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases
        %standard-phases
        (delete 'validate-runpath)
        (replace 'install
                 (lambda* (#:key source inputs outputs #:allow-other-keys)
                   (invoke "python3" "-m" "installer"
                           (string-append "aqt-" #$version "-py3-none-any.whl")
                           "--destdir" (assoc-ref outputs "out")
                           "-p" "")
                   (copy-file (assoc-ref inputs "anki")
                              (string-append "anki-" #$version "-cp39-abi3-manylinux_2_36_x86_64.whl"))
                   (invoke "python3" "-m" "installer"
                           (string-append "anki-" #$version "-cp39-abi3-manylinux_2_36_x86_64.whl")
                           "--destdir" (assoc-ref outputs "out")
                           "-p" "")))
        (add-after 'install 'install-fixes
                   (lambda _
                     (let* ((python-path (list (getenv "GUIX_PYTHONPATH")
                                               (string-append
                                                #$output
                                                "/lib/python3.11/site-packages"))))
                       (wrap-program (string-append #$output "/bin/anki")
                                     `("PYTHONPATH" prefix ,python-path))))) )))
   (propagated-inputs
    (list python
          python-pyqt-6
          python-pyqt6-sip
          python-beautifulsoup4
          python-flask
          python-flask-cors
          python-jsonschema
          python-markdown
          python-orjson
          python-protobuf-6
          python-pyqtwebengine-6
          python-send2trash
          python-waitress
          python-decorator
          python-distro
          python-requests
          python-pysocks
          qtsvg
          nss-certs))
   (native-inputs (list python python-installer))
   (supported-systems '("x86_64-linux"))
   (home-page "https://apps.ankiweb.net")
   (synopsis "Spaced repetition program")
   (description
    "Anki's shared backend and web components, and the Qt frontend.")
   (license license:agpl3+)))
