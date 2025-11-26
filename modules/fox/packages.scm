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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages check)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu))

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


(define-public rimerc-zrm
  (package
   (name "rimerc-zrm")
   (version "master")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.southfox.me/southfox/rimerc-zrm.git")
                  (commit "c95f7896c838fa3a1b9c01269a68a819bcf97a55")))
            (sha256
             (base32
              "0raj1fkmxwxpk9sxb32ap5asjv2h577jpnsmil4x1a7nmrbm23fd"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan #~'(("." "share/fctix5/rime"))))
   (home-page "https://github.com/mutoe/rime")
   (synopsis "Rime arm config")
   (description
    "Rime zrm config.")
   (license expat)))

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
