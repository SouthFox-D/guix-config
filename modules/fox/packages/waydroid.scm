(define-module (fox packages waydroid)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages gtk)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject))

(define-public libglibutil
  (package
   (name "libglibutil")
   (version "1.0.80")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/sailfishos/libglibutil")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1bkk4k79qw19p5j0w2iq6jywcsrg8d8ickx1905h5faf5dqkp7y2"))))
   (build-system gnu-build-system)
   (arguments
    (list #:make-flags #~(list (string-append "CC="
                                              #$(cc-for-target))
                               (string-append "DESTDIR="
                                              #$output))
          #:phases #~(modify-phases %standard-phases
                                    (delete 'configure)
                                    (add-after 'unpack 'remove-usr-prefix
                                               (lambda* _
                                                 (substitute* "libglibutil.pc.in"
                                                              (("/usr/include") (string-append #$output
                                                                                               "/include")))
                                                 (substitute* "Makefile"
                                                              (("usr/") ""))))
                                    (add-after 'install 'install-dev
                                               (lambda* _
                                                 (invoke "make" "install-dev"
                                                         (string-append "DESTDIR="
                                                                        #$output))))
                                    (replace 'check
                                             (lambda* (#:key tests? #:allow-other-keys)
                                               (when tests?
                                                 (chdir "test")
                                                 (invoke "make"
                                                         (string-append "CC="
                                                                        #$(cc-for-target)))
                                                 (chdir "..")))))))
   (native-inputs (list pkg-config))
   (inputs (list glib))
   (home-page "https://git.sailfishos.org/mer-core/libglibutil")
   (synopsis "GLib utilites")
   (description "This package provides library of glib utilities.")
   (license license:bsd-3)))

(define-public libgbinder
  (package
   (name "libgbinder")
   (version "1.1.43")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mer-hybris/libgbinder")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1wv9vk58daq2ibg8mizcylp5f99fhkbbqww2gb85axlmcg6m12bb"))))
   (build-system gnu-build-system)
   (arguments
    (list #:make-flags #~(list (string-append "CC="
                                              #$(cc-for-target))
                               (string-append "DESTDIR="
                                              #$output))
          #:phases #~(modify-phases %standard-phases
                                    (delete 'configure)
                                    (add-after 'unpack 'fix-pkg-config-in
                                               (lambda* _
                                                 (substitute* "Makefile"
                                                              (("usr/") ""))
                                                 (substitute* "libgbinder.pc.in"
                                                              (("@libdir@") (string-append #$output "/lib"))
                                                              (("/usr/include") (string-append #$output
                                                                                               "/include")))))
                                    (add-after 'install 'install-dev
                                               (lambda* _
                                                 (invoke "make" "install-dev"
                                                         (string-append "DESTDIR="
                                                                        #$output))))
                                    (replace 'check
                                             (lambda* (#:key tests? #:allow-other-keys)
                                               (when tests?
                                                 (chdir "test")
                                                 (invoke "make"
                                                         (string-append "CC="
                                                                        #$(cc-for-target)))
                                                 (chdir "..")))))))
   (native-inputs (list bison flex pkg-config))
   (inputs (list glib libglibutil))
   (home-page "https://github.com/mer-hybris/libgbinder")
   (synopsis "GLib-style interface to binder")
   (description
    "This package provides GLib-style interface to binder:
@enumerate
@item Integration with GLib event loop
@item Detection of 32 vs 64 bit kernel at runtime
@item Asynchronous transactions that don't block the event thread
@item Stable service manager and low-level transation APIs
@end enumerate")
   (license license:bsd-3)))

(define-public python-gbinder
  (package
   (name "python-gbinder")
   (version "1.3.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/waydroid/gbinder-python")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1wl42xxqx6nkjb53b22j2676x3fyh9zqmzn37mnypyh1zh14qj6g"))))
   (build-system python-build-system)
   (arguments
    (list #:phases #~(modify-phases %standard-phases
                                    (replace 'build
                                             (lambda* _
                                               (invoke "python" "setup.py" "build_ext"
                                                       "--inplace"))))))
   (native-inputs (list python-cython-0 pkg-config))
   (inputs (list glib libgbinder libglibutil))
   (home-page "https://github.com/erfanoabdi/gbinder-python")
   (synopsis "Python bindings for libgbinder")
   (description "This package provides Python bindings for libgbinder.")
   (license license:gpl3+)))

(define-public waydroid
  (package
   (name "waydroid")
   (version "1.6.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/waydroid/waydroid")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05a7zr5xjc11hl5pg6yq1rrmqkpnc0jnz9111h810dsg78ajfiy3"))))
   (build-system gnu-build-system)
   (arguments
    (list #:make-flags
          #~(list "USE_SYSTEMD=0"
                  (string-append "PREFIX=" #$output)
                  (string-append "SYSCONFDIR=" #$output "/etc"))
          #:phases
          #~(modify-phases
             %standard-phases
             (delete 'configure)
             (delete 'check)
             (add-after 'unpack 'unpack-fixes
                        (lambda _
                          (substitute* '("tools/helpers/run.py"
                                         "tools/helpers/lxc.py")
                                       (("\"sh\"") (string-append "\"" #$bash-minimal "/bin/sh" "\""))
                                       (("\"lxc-info\"") (string-append "\"" #$lxc "/bin/lxc-info" "\""))
                                       (("\"lxc-start\"") (string-append "\"" #$lxc "/bin/lxc-start" "\""))
                                       (("\"lxc-stop\"") (string-append "\"" #$lxc "/bin/lxc-stop" "\""))
                                       (("\"lxc-freeze\"") (string-append "\"" #$lxc "/bin/lxc-freeze" "\""))
                                       (("\"lxc-unfreeze\"") (string-append "\"" #$lxc "/bin/lxc-unfreeze" "\""))
                                       (("\"lxc-attach\"") (string-append "\"" #$lxc "/bin/lxc-attach" "\"")))
                          (substitute* '("dbus/id.waydro.Container.service")
                                       (("/usr/bin/waydroid") (string-append #$output "/bin/waydroid" )))))
             (add-after 'install 'install-fixes
                        (lambda _
                          (let* ((paths (list (string-append #$iptables "/bin")
                                              (string-append #$iptables "/sbin")
                                              (string-append #$iproute "/bin")
                                              (string-append #$dnsmasq "/bin")
                                              (string-append #$glibc "/bin")
                                              (string-append #$lxc "/bin")
                                              (string-append #$wl-clipboard "/bin")
                                              (string-append #$dnsmasq "/sbin")))
                                 (python-path (map (lambda (i)
                                                     (string-append i "/lib/python3.11/site-packages"))
                                                   (list #$(this-package-input "python-dbus-python")
                                                         #$(this-package-input "python-gbinder")
                                                         #$(this-package-input "python-pygobject")
                                                         #$(this-package-input "python-pyclip")))))
                            (wrap-program (string-append #$output "/lib/waydroid/data/scripts/waydroid-net.sh")
                                          `("PATH" prefix ,paths))
                            (wrap-program (string-append #$output "/bin/waydroid")
                                          `("PYTHONPATH" prefix ,python-path))
                            (wrap-program (string-append #$output "/bin/waydroid")
                                          `("GI_TYPELIB_PATH" prefix ,(list
                                                                       (string-append
                                                                        #$(this-package-input "glib")
                                                                        "/lib/girepository-1.0"))))
                            (wrap-program (string-append #$output "/bin/waydroid")
                                          '("SSL_CERT_DIR" ":" = ("/etc/ssl/certs")))))))))
   (native-inputs (list gobject-introspection))
   (propagated-inputs
    (list gawk
          glib
          gtk
          kmod
          lxc
          python
          python-dbus-python
          python-gbinder
          python-pyclip
          python-pygobject
          util-linux
          wl-clipboard))
   (home-page "https://waydro.id")
   (synopsis "Container-based approach to boot a full Android system")
   (description
    "Waydroid uses Linux namespaces @code{(user, pid, uts, net,
mount, ipc)} to run a full Android system in a container and provide Android
applications.  The Android inside the container has direct access to needed
underlying hardware.  The Android runtime environment ships with a minimal
customized Android system image based on LineageOS.  The used image is
currently based on Android 11.")
   (license license:gpl3)))

(define-public python-pfzy
  (package
   (name "python-pfzy")
   (version "0.3.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kazhala/pfzy")
           (commit version)))
     (sha256
      (base32 "12d5ff1cg1a2qjvibhs4banwsawdna9glp827j7l8kqznp4by5pq"))))
   (build-system pyproject-build-system)
   (arguments `(#:tests? #f))
   (native-inputs (list python-poetry-core))
   (home-page "https://github.com/kazhala/pfzy")
   (synopsis "Python port of the fzy fuzzy string matching algorithm")
   (description "Python port of the fzy fuzzy string matching algorithm.")
   (license license:expat)))

(define-public python-inquirerpy
  (package
   (name "python-inquirerpy")
   (version "0.3.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kazhala/InquirerPy")
           (commit version)))
     (sha256
      (base32 "01s1wpsfsjxd1vpvhrz9b5314fml8kg11a3fiqnrzqqlf5j33782"))))
   (build-system pyproject-build-system)
   (native-inputs (list python-poetry-core))
   (propagated-inputs
    (list
     python-prompt-toolkit
     python-pfzy))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/kazhala/InquirerPy")
   (synopsis
    "Python port of Inquirer.js (A collection of common interactive command-line user interfaces)")
   (description
    "Python port of Inquirer.js (A collection of common interactive command-line user
interfaces).")
   (license license:expat)))

(define-public waydroid-script
  (package
   (name "waydroid-script")
   (version "main")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/casualsnek/waydroid_script")
           (commit "ddaa6b190f98b250e433c14946de7b69713a4b94")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0ka4rnc4s8zl40hcwlzrz98qfc8sz3i1r9y7gpq7g4ja56gy6dwd"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("bin/" "bin/"))
     #:phases
     #~(modify-phases
        %standard-phases
        (delete 'validate-runpath)
        (add-after 'unpack 'move
                   (lambda _
                     (mkdir-p (string-append #$output "/libexec/waydroid_script"))
                     (copy-recursively "." (string-append #$output "/libexec/waydroid_script"))
                     (delete-file-recursively "bin/")
                     (mkdir "bin/")
                     (mkdir (string-append #$output "/bin"))
                     (symlink (string-append #$output "/libexec/waydroid_script/main.py")
                              (string-append #$output "/bin/waydroid_script"))))
        (add-after 'install 'wrap
                   (lambda _
                     (let* ((site-packages (map (lambda (i)
                                                  (string-append i "/lib/python3.11/site-packages"))
                                                (list #$(this-package-input "python-tqdm")
                                                      #$(this-package-input "python-requests")
                                                      #$(this-package-input "python-inquirerpy")
                                                      #$(this-package-input "python-prompt-toolkit")
                                                      #$(this-package-input "python-wcwidth")
                                                      #$(this-package-input "python-pfzy")
                                                      #$(this-package-input "python-certifi")
                                                      #$(this-package-input "python-charset-normalizer")
                                                      #$(this-package-input "python-charset-normalizer")
                                                      #$(this-package-input "python-idna")
                                                      #$(this-package-input "python-urllib3")))))
                       (wrap-program (string-append #$output "/libexec/waydroid_script/main.py")
                                     `("PYTHONPATH" = ,site-packages))))))))
   (propagated-inputs
    (list python
          python-tqdm
          python-requests
          python-inquirerpy
          python-prompt-toolkit
          python-wcwidth
          python-pfzy
          python-certifi
          python-charset-normalizer
          python-idna
          python-urllib3))
   (home-page "https://github.com/casualsnek/waydroid_script")
   (synopsis "Script to add GApps and other stuff to Waydroid")
   (description
    "Python Script to add OpenGapps, Magisk, libhoudini translation library and
libndk translation library to waydroid !")
   (license license:gpl3)))
