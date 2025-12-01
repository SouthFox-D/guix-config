(define-module (fox packages binary)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (fox packages licenses)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system copy))

(define-public zellij-bin
  (package
   (name "zellij-bin")
   (version "0.43.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/zellij-org/zellij/releases/download/v"
                  version "/zellij-" (cond ((target-aarch64?)
                                            "aarch64")
                                           ((target-x86-64?)
                                            "x86_64")
                                           (else ""))
                  "-unknown-linux-musl.tar.gz"))
            (sha256
             (base32 (cond ((target-aarch64?)
                            "0rdi60lz1zmhg76xg874hipnc8y8bpwisb8nav8n4b0wyvailcij")
                           ((target-x86-64?)
                            "0nnp41q21n2mfbkvj4qmi1p0p4jdkv9arnasz0z2jn2mxzprh7al")
                           (else ""))))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("zellij" "bin/"))))
   (supported-systems (list "aarch64-linux" "x86_64-linux"))
   (home-page "https://github.com/zellij-org/zellij")
   (synopsis "terminal workspace with batteries included")
   (description "Terminal workspace with batteries included.")
   (license license:expat)))

(define-public hugo-bin
  (package
   (name "hugo-bin")
   (version "0.152.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gohugoio/hugo" "/releases/download/v"
                  version "/hugo_extended_" version "_linux-amd64.tar.gz"))
            (sha256
             (base32
              "09wc9p5x4knnb6q2b8b99fwj3z2hv83ybns4jvn6k17nynywyss1"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("hugo" "bin/"))
          #:phases
          #~(modify-phases %standard-phases
                           (delete 'strip)
                           (add-after 'install 'patch-elf
                                      (lambda _
                                        (let ((hugo (string-append #$output "/bin/hugo")))
                                          (invoke "patchelf" "--set-interpreter"
                                                  (string-append #$(this-package-input "glibc")
                                                                 #$(glibc-dynamic-linker))
                                                  hugo)
                                          (invoke "patchelf" "--set-rpath"
                                                  (string-append (ungexp (this-package-input "gcc")
                                                                         "lib")
                                                                 "/lib")
                                                  hugo)))))))
   (supported-systems '("x86_64-linux"))
   (native-inputs (list patchelf-0.16))
   (inputs (list `(,gcc "lib") glibc))
   (home-page "https://gohugo.io/")
   (synopsis "Static site generator")
   (description
    "Hugo is a static site generator written in Go, optimized for speed and
designed for flexibility.")
   (license license:asl2.0)
   (properties '((upstream-name . "hugo")))))

(define-public frp-bin
  (package
   (name "frp-bin")
   (version "0.65.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/fatedier/frp" "/releases/download/v"
                  version "/frp_" version "_linux_amd64.tar.gz"))
            (sha256
             (base32
              "1mg6nwaazvl6gapwjsllc4c0d32nqw84rnh9k65g8wppzp2xikjj"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("frps" "bin/")
                             ("frpc" "bin/"))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/fatedier/frp")
   (synopsis "Fast reverse proxy")
   (description
    "A fast reverse proxy to help you expose a local server behind a NAT or firewall to the
internet.")
   (properties '((upstream-name . "frp")))
   (license license:asl2.0)))

(define-public v2rayn-bin
  (package
   (name "v2rayn-bin")
   (version "7.16.4")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/2dust/v2rayN" "/releases/download/"
                  version "/v2rayN-linux-64.zip"))
            (sha256
             (base32
              "1y3p6mr3apv1bzr10xp6sd7plff5l6l6d3zrm6j9lp2wkj4c63l7"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("./" "opt/v2rayn-bin"))))
   (supported-systems '("x86_64-linux"))
   (native-inputs (list unzip))
   (home-page "https://github.com/2dust/v2rayN")
   (synopsis "V2ray GUI client")
   (description
    "A GUI client for Windows, Linux and macOS, support Xray and sing-box
and others.")
   (properties '((upstream-name . "v2rayN")))
   (license license:gpl3)))

(define-public nyxt-bin
  (package
   (name "nyxt-bin")
   (version "4.0.0-pre-release-13")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/atlas-engineer/nyxt" "/releases/download/"
                  version "/Linux-Nyxt-x86_64.tar.gz"))
            (sha256
             (base32
              "1hsz8xrs3pn00rb5z07dcm5xnbwplf6pa8zz5xkp5ag7bcnj0k7n"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("Nyxt-x86_64.AppImage" "opt/nyxt-bin/nyxt"))
          #:phases
          #~(modify-phases %standard-phases
                           (delete 'validate-runpath)
                           (delete 'make-dynamic-linker-cache))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://nyxt-browser.com/")
   (synopsis "Extensible web-browser in Common Lisp")
   (description
    "Nyxt is a keyboard-oriented, extensible web-browser designed for power users.
The application has familiar Emacs and VI key-bindings and is fully configurable
and extensible in Common Lisp.")
   (license license:bsd-3)))

(define-public sing-box-bin
  (package
    (name "sing-box-bin")
    (version "1.12.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SagerNet/sing-box/releases/download/v"
                    version "/sing-box-" version "-linux-"
                    (cond ((target-aarch64?)
                           "arm64")
                          ((target-x86-64?)
                           "amd64")
                          (else "")) ".tar.gz"))
              (sha256
               (base32 (cond ((target-aarch64?)
                              "1xy8j9c6j13jrpkrppxb27s9gsxrkn2m1m4hxamrdvqpnivgxnpv")
                             ((target-x86-64?)
                              "17w8bxvrp6lky02n93a7kac2gv8qcxcl684nhb5m9p57z6r3q43w")
                             (else ""))))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("sing-box" "bin/"))))
    (supported-systems (list "aarch64-linux" "x86_64-linux"))
    (home-page "https://sing-box.sagernet.org/")
    (synopsis "Universal proxy platform")
    (description
     "Sing-box is a customizable and univsersal proxy platform that can be used to
create network proxy servers, clients and transparent proxies.")
    (license license:gpl3+)
    (properties '((upstream-name . "sing-box")))))

(define-public opentabletdriver-bin
  (package
   (name "opentabletdriver-bin")
   (version "0.6.6.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/OpenTabletDriver/OpenTabletDriver/releases/download/v"
                  version "/opentabletdriver-" version "-x64.tar.gz"))
            (sha256
             (base32
              "0kp3yhrr959rkw1l4pi5ysw014kd7dnrs78g189hp47mja6vqi59"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("opentabletdriver/usr/local/lib/opentabletdriver/OpenTabletDriver.Daemon" "bin/otd-daemon")
         ("opentabletdriver/usr/local/lib/opentabletdriver/OpenTabletDriver.UX.Gtk" "bin/otd-gui")
         ("opentabletdriver/usr/local/lib/opentabletdriver/OpenTabletDriver.Console" "bin/otd")
         ("opentabletdriver/usr/local/share/libinput/30-vendor-opentabletdriver.quirks"
          "usr/share/libinput/30-vendor-opentabletdriver.quirks")
         ("opentabletdriver/usr/local/lib/modprobe.d/99-opentabletdriver.conf"
          "usr/lib/modprobe.d/99-opentabletdriver.conf")
         ("opentabletdriver/usr/local/lib/modules-load.d/opentabletdriver.conf"
          "usr/lib/modules-load.d/opentabletdriver.conf")
         ("opentabletdriver/etc/udev/rules.d/70-opentabletdriver.rules"
          "usr/lib/udev/rules.d/70-opentabletdriver.rules"))
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'strip)
                      (delete 'validate-runpath)
                      (replace 'unpack
                               (lambda* (#:key source #:allow-other-keys)
                                 (invoke "tar" "-xvf" source))))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://opentabletdriver.net/")
   (synopsis "Open source, cross-platform, user-mode tablet driver")
   (description
    "Open source, cross-platform, user-mode tablet driver")
   (license license:lgpl3)))

(define-public resilio-sync-bin
  (package
   (name "resilio-sync-bin")
   (version "3.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://download-cdn.resilio.com/stable/linux/arm64/0/resilio-sync_"
                  (cond ((target-aarch64?)
                         "arm64")
                        ((target-x86-64?)
                         "x64")
                        (else "")) ".tar.gz"))
            (sha256
             (base32 (cond ((target-aarch64?)
                            "0dp19xbiydrdh9sgl3qnb0khrxva44j5kll5csqrz451shw0dhyd")
                           ((target-x86-64?)
                            "1w7hf32iajiw6ab6g2db9d7fml848w8s5375zbjjl7njnd0xvziw")
                           (else ""))))))
   (build-system copy-build-system)
   (arguments (list #:install-plan #~'(("rslsync" "bin/"))
                    #:phases
                    #~(modify-phases %standard-phases
                                     (delete 'strip)
                                     (delete 'validate-runpath)
                                     (add-after 'install 'patch-elf
                                                (lambda _
                                                  (let ((rslsync (string-append #$output "/bin/rslsync")))
                                                    (invoke "patchelf" "--set-rpath"
                                                            (string-append #$(this-package-input "libxcrypt")
                                                                           "/lib"
                                                                           )
                                                            rslsync)
                                                    (invoke "patchelf" "--set-interpreter"
                                                            (string-append #$(this-package-input "glibc")
                                                                           #$(glibc-dynamic-linker))
                                                            rslsync)))))))
   (supported-systems (list "aarch64-linux" "x86_64-linux"))
   (inputs (list glibc libxcrypt))
   (native-inputs (list patchelf-0.16))
   (home-page "https://www.resilio.com/sync/")
   (synopsis "Personal file sync and share powered by P2P")
   (description
    "Save and share life’s most important moments—photos, videos, music, PDFs,
documents, and more—securely across all your devices.")
   (license (nonfree "https://www.resilio.com/legal/copyright/"))))

(define-public yay-bin
  (package
   (name "yay-bin")
   (version "12.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Jguer/yay" "/releases/download/v"
                  version "/yay_" version "_x86_64.tar.gz"))
            (sha256
             (base32
              "0r5675c7hq2kfy82yj2zrn6dp46mrf1161jgs893b6v15sr71nl9"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("yay" "bin/"))
          #:phases
          #~(modify-phases %standard-phases
                           (delete 'strip)
                           ;; depends host pacman
                           (delete 'validate-runpath))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/Jguer/yay")
   (synopsis "Pacman wrapper and AUR helper written in go")
   (description
    "Yet another yogurt. Pacman wrapper and AUR helper written in go.
Pre-compiled.")
   (properties '((upstream-name . "yay")))
   (license license:gpl3+)))
