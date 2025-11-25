(define-module (fox packages)
  #:use-module (guix build utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages games)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu home services)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu services))

(define-public oh-my-zsh
  (package
   (name "oh-my-zsh")
   (version "master")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ohmyzsh/ohmyzsh.git")
                  (commit "8a4d6fc0a2b5586f093fb2b96b51e2141f643284")))
            (sha256
             (base32
              "01sp5pbn7zqi30kpni6npbcp3jfs55gfcqqzlfa6mhd7djh4v3si"))))
   (build-system copy-build-system)
   (synopsis "framework for managing your zsh configuration")
   (description "🙃A delightful community-driven (with 2,400+ contributors)
framework for managing your zsh configuration. Includes 300+ optional plugins
(rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes
to spice up your morning, and an auto-update tool that makes it easy to keep up
with the latest updates from the community.")
   (home-page "https://ohmyz.sh/")
   (license expat)))

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
     #~'(("zellij" "bin/"))
     #:phases
     #~(modify-phases %standard-phases
                      (replace 'unpack
                               (lambda* (#:key source #:allow-other-keys)
                                 (invoke "tar" "-xvf" source))))))
   (supported-systems (list "aarch64-linux" "x86_64-linux"))
   (home-page "https://github.com/zellij-org/zellij")
   (synopsis "terminal workspace with batteries included")
   (description "Terminal workspace with batteries included.")
   (license expat)))

(define-public hugo-bin
  (package
   (name "hugo-bin")
   (version "0.147.9")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gohugoio/hugo" "/releases/download/v"
                  version "/hugo_extended_" version "_linux-amd64.tar.gz"))
            (sha256
             (base32
              "1z4qiiqz23qi7k4z2kdbn4013i0nf3n2rakba36n74lca4jpq1yb"))))
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
   (license asl2.0)
   (properties '((upstream-name . "hugo")))))

(define-public frp-bin
  (package
   (name "frp-bin")
   (version "0.63.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/fatedier/frp" "/releases/download/v"
                  version "/frp_" version "_linux_amd64.tar.gz"))
            (sha256
             (base32
              "138pfqlkhrq041qh1lksczxvbl9imvhaz6hwgxw7f764cl8xglnh"))))
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
   (license asl2.0)
   (properties '((upstream-name . "frp")))))

(define-public v2rayn-bin
  (package
   (name "v2rayn-bin")
   (version "7.12.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/2dust/v2rayN" "/releases/download/"
                  version "/v2rayN-linux-64.zip"))
            (sha256
             (base32
              "03nh7xyqdyfmkgvkw276q6w9ykm161qyr01w16vva4hdkphd7x67"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("v2rayN-linux-64" "opt/v2rayn-bin"))
          #:phases
          #~(modify-phases %standard-phases
                           (replace 'unpack
                                    (lambda* (#:key source #:allow-other-keys)
                                      (invoke "unzip" source))))))
   (supported-systems '("x86_64-linux"))
   (native-inputs (list unzip))
   (home-page "https://github.com/2dust/v2rayN")
   (synopsis "V2ray GUI client")
   (description
    "A GUI client for Windows, Linux and macOS, support Xray and sing-box
and others.")
   (license gpl3)))

(define-public anki-bin
  (package
   (name "anki-bin")
   (version "25.02.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/ankitects/anki" "/releases/download/"
                  version "/anki-" version "-linux-qt6.tar.zst"))
            (sha256
             (base32
              "0l0jilmm12bsf1jcvn2kjzjdk4i0csz0bzhy74mdmbiyb17nm0f1"))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("anki-25.02.5-linux-qt6/" "opt/anki-bin"))
          #:phases
          #~(modify-phases %standard-phases
                           (replace 'unpack
                                    (lambda* (#:key source #:allow-other-keys)
                                      (invoke "tar" "-xvf" source))))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://apps.ankiweb.net")
   (synopsis "Spaced repetition program")
   (description
    "Anki's shared backend and web components, and the Qt frontend.")
   (license agpl3+)))

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
   (license bsd-3)))

(define license (@@ (guix licenses) license))

(define* (nonfree uri #:optional (comment ""))
  "Return a nonfree license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Nonfree"
           uri
           (string-append
            "This a nonfree license.  Check the URI for details.  "
            comment)))

(define-public zerotier
  (package
   (name "zerotier")
   (version "1.14.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/zerotier/ZeroTierOne")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0dxm67c06hg538z8adxgj5zqxmgrwhnlwznpi3sk2qvfms6zzvhg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags (list "ZT_SSO_SUPPORTED=0") ; We don't need SSO/OIDC
      #:phases
      (modify-phases %standard-phases
                     ;; There is no ./configure
                     (delete 'configure)
                     (replace 'check
                              (lambda* (#:key make-flags #:allow-other-keys)
                                (apply invoke "make" "selftest" make-flags)
                                (invoke "./zerotier-selftest")))
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let* ((out (assoc-ref outputs "out"))
                                       (sbin (string-append out "/sbin"))
                                       (lib (string-append out "/lib"))
                                       (man (string-append out "/share/man"))
                                       (zerotier-one-lib (string-append lib "/zerotier-one")))
                                  (mkdir-p sbin)
                                  (install-file "zerotier-one" sbin)
                                  (with-directory-excursion sbin
                                                            (symlink (string-append sbin "/zerotier-one") "zerotier-cli")
                                                            (symlink (string-append sbin "/zerotier-one") "zerotier-idtool"))

                                  (mkdir-p zerotier-one-lib)
                                  (with-directory-excursion zerotier-one-lib
                                                            (symlink (string-append sbin "/zerotier-one") "zerotier-one")
                                                            (symlink (string-append sbin "/zerotier-one") "zerotier-cli")
                                                            (symlink (string-append sbin "/zerotier-one") "zerotier-idtool"))

                                  (mkdir-p (string-append man "/man8"))
                                  (install-file "doc/zerotier-one.8" (string-append man "/man8"))

                                  (mkdir-p (string-append man "/man1"))
                                  (for-each (lambda (man-page)
                                              (install-file man-page (string-append man "/man1")))
                                            (list "doc/zerotier-cli.1"
                                                  "doc/zerotier-idtool.1"))
                                  #t))))))
   (home-page "https://github.com/zerotier/ZeroTierOne")
   (synopsis "Smart programmable Ethernet switch for planet Earth")
   (description "It allows all networked devices, virtual machines,
containers, and applications to communicate as if they all reside in the same
physical data center or cloud region.

This is accomplished by combining a cryptographically addressed and secure
peer to peer network (termed VL1) with an Ethernet emulation layer somewhat
similar to VXLAN (termed VL2).  Our VL2 Ethernet virtualization layer includes
advanced enterprise SDN features like fine grained access control rules for
network micro-segmentation and security monitoring.")
   (license (nonfree "https://mariadb.com/bsl11/"))))

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
    (license gpl3+)
    (properties '((upstream-name . "sing-box")))))

(define-public rimerc-zrm
  (package
   (name "rimerc-zrm")
   (version "01cf4a8f58c00b4b3837be65a997e8274723ecda")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mutoe/rime.git")
                  (commit "01cf4a8f58c00b4b3837be65a997e8274723ecda")))
            (sha256
             (base32
              "03l8qkn67k97gbf5940vm72li54xkh41sz65mfjns0rz2h1xx0wj"))
            (patches (list (local-file "patches/0001-feat-remove-english-support.patch")))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan #~'(("." "share/fctix5/rime"))
     #:phases
     #~(modify-phases
        %standard-phases
        (add-after 'unpack 'patch
                   (lambda _
                     (delete-file-recursively ".vscode")
                     (delete-file-recursively "example_images")
                     (delete-file "english.dict.yaml")
                     (delete-file "english.schema.yaml"))))))
   (home-page "https://github.com/mutoe/rime")
   (synopsis "Rime arm config")
   (description
    "Rime zrm config.")
   (license expat)))

(define-public yay-bin
  (package
   (name "yay-bin")
   (version "12.5.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Jguer/yay" "/releases/download/v"
                  version "/yay_" version "_x86_64.tar.gz"))
            (sha256
             (base32
              "1pysficjq375iad2b89ck94b1pnl2d2fdazdxy4xcm4h2qyfkf6h"))))
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
   (license gpl3+)
   (properties '((upstream-name . "yay")))))

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
   (license bsd-3)))

(define-public libgbinder
  (package
   (name "libgbinder")
   (version "1.1.42")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/mer-hybris/libgbinder")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1in95drgr647q785ljiyi2wqp5fws1iqi0bjs49qx359c019z73z"))))
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
   (license bsd-3)))

(define-public python-gbinder
  (package
   (name "python-gbinder")
   (version "1.1.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/erfanoabdi/gbinder-python")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0wdpf6b6b3cmd42j1qkfp19y9bdymy5dv12r261gkhvxxh160zym"))))
   (build-system python-build-system)
   (arguments
    (list #:phases #~(modify-phases %standard-phases
                                    (replace 'build
                                             (lambda* _
                                               (invoke "python" "setup.py" "build_ext"
                                                       "--inplace" "--cython"))))))
   (native-inputs (list python-cython-0 pkg-config))
   (inputs (list glib libgbinder libglibutil))
   (home-page "https://github.com/erfanoabdi/gbinder-python")
   (synopsis "Python bindings for libgbinder")
   (description "This package provides Python bindings for libgbinder.")
   (license gpl3+)))

(define-public waydroid
  (package
   (name "waydroid")
   (version "1.5.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/waydroid/waydroid")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "050cn84ambjd02cnszp9qqgzm62x14jxg9jp7fxrzbv6qps8k2rb"))))
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
   (license gpl3)))

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
   (license expat)))

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
     python-pfzy
     ))
   (arguments `(#:tests? #f))
   (home-page "https://github.com/kazhala/InquirerPy")
   (synopsis
    "Python port of Inquirer.js (A collection of common interactive command-line user interfaces)")
   (description
    "Python port of Inquirer.js (A collection of common interactive command-line user
interfaces).")
   (license expat)))

(define-public waydroid-script
  (package
   (name "waydroid-script")
   (version "main")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/casualsnek/waydroid_script")
           (commit "3e344b360f64f4a417c4f5e9a3b1aae3da9fdfb9")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0djfybvzj5gz8xrhqfx1187c1qs4k57q67m7k2jqvpbki7agb0lp"))))
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
                                                      #$(this-package-input "python-urllib3")
                                                      ))))
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
   (license gpl3)))

(define-public libfprint-focaltech
  (package
   (name "libfprint-focaltech")
   (version "main")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://raw.githubusercontent.com/ftfpteams/focaltech-linux-fingerprint-driver"
                  "/3d47a5aef6535736495fcc0f98645a98f14b9af9/Fedora_Redhat/"
                  "libfprint-2-2_1.94.4+tod1_suse_all_x64_20250219.install"))
            (file-name (string-append name ".tar.gz"))
            (sha256
             (base32
              "1hxgj6qsdxn3i5hxqm9m8awwn1y3ldy70c91xp1nps1x0vrpryx3"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (replace 'unpack
                               (lambda* (#:key source #:allow-other-keys)
                                 (with-output-to-file "data.tar.gz"
                                   (lambda ()
                                     (invoke "sed" "1,/^main \\$@/d" source)))
                                 (display (invoke "ls" "-lah"))
                                 (invoke "tar" "-zxf" "data.tar.gz")
                                 (mkdir-p (string-append #$output "/usr/lib64"))
                                 (copy-file
                                  "usr/lib64/libfprint-2.so.2.0.0"
                                  (string-append #$output "/usr/lib64/libfprint-2.so.2.0.0"))))
                      (replace 'install
                               (lambda _
                                 (symlink (string-append #$output "/usr/lib64/libfprint-2.so.2.0.0")
                                          (string-append #$output "/usr/lib64/libfprint-2.so.2"))
                                 (symlink (string-append #$output "/usr/lib64/libfprint-2.so.2.0.0")
                                          (string-append #$output "/usr/lib64/libfprint-2.so")))))))
   (supported-systems '("x86_64-linux"))
   (native-inputs (list tar coreutils))
   (home-page "https://github.com/ftfpteams/focaltech-linux-fingerprint-driver")
   (synopsis "Focaltech moh fingerprint driver")
   (description
    "focaltech moh fingerprint driver for ubuntu, support with vid:0x2808,
pid: 0x9338, 0xd979, 0xc652, 0xa959, 0x0579 ")
   (license expat)))

(define %minimal-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "en_US")
   #:name "glibc-utf8-locales"))

(define-public minimal-glibc-locales
  (package
   (name "minimal-glibc-locales")
   (version "master")
   (source (plain-file "hello" "Hello World!"))
   (build-system copy-build-system)
   (arguments (list
               #:install-plan #~'(("hello" "share/"))
               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-after 'unpack 'move-locales
                             (lambda _
                               (mkdir-p (string-append #$output "/share"))
                               (invoke
                                "cp" "-r"
                                (string-append #$(this-package-input "glibc-utf8-locales")
                                               "/lib")
                                (string-append #$output "/")))))))
   (inputs (list %minimal-glibc-locales))
   (synopsis "x")
   (description "x")
   (home-page "x")
   (license expat)))

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
   (license lgpl3)))

(define-public cataclysm-dda-experimental
  (package
   (inherit cataclysm-dda)
   (name "cataclysm-dda-experimental")
   (version "2025-10-25")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/CleverRaven/Cataclysm-DDA")
           (commit "c30e3e6e1c63cc7132b748ea25a4dcd3f5b99244")))
     (sha256
      (base32 "1gmi2psgchzp62kgbvg6bc38qqksvbngn5n64q02bcp3izpni56a"))
     (file-name (git-file-name name version))))
   (arguments
    (list
     #:make-flags
     #~(list (string-append "PREFIX=" #$output)
             "USE_HOME_DIR=1" "DYNAMIC_LINKING=1" "RELEASE=1" "WARNINGS=-w"
             "LOCALIZE=1" "LANGUAGES=all")
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure))
     #:tests? #f))
   (inputs
    (list ncurses zlib))
   (outputs '("out"))))

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
                                                            rslsync)
                                                    ))))
                    ))
   (supported-systems (list "aarch64-linux" "x86_64-linux"))
   (inputs (list glibc libxcrypt))
   (native-inputs (list patchelf-0.16))
   (home-page "https://www.resilio.com/sync/")
   (synopsis "Personal file sync and share powered by P2P")
   (description
    "Save and share life’s most important moments—photos, videos, music, PDFs,
documents, and more—securely across all your devices.")
   (license (nonfree "https://www.resilio.com/legal/copyright/"))))

(define-public dnsmasq-china-list-smartdns
  (package
   (name "dnsmasq-china-list-smartdns")
   (version "2025-11-25")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/felixonmars/dnsmasq-china-list.git")
           (commit "a4036bc84ab76850b8bb8a53742899b24bb6c0de")))
     (sha256
      (base32 "09pn5r9mm21a44k0i6yrvyvl9d03hkhjjfyx5flqv9aw7ajam8lg"))
     (file-name (git-file-name name version))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f
     #:make-flags #~(list "SERVER=proximal" "smartdns")
     #:phases
     #~(modify-phases
        %standard-phases
        (delete 'configure)
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out")))
                     (for-each
                      (lambda (file)
                        (install-file file out))
                      (find-files "./" "\\.smartdns.conf$"))))))))
   (home-page "https://github.com/felixonmars/dnsmasq-china-list")
   (synopsis "Chinese-specific DNS configuration")
   (description
    "Chinese-specific configuration to improve your favorite DNS server.
Best partner for chnroutes. ")
   (license wtfpl2)))
