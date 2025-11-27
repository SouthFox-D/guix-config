(define-module (fox packages networking)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (fox packages licenses)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go))

(define-public zerotier
  (package
   (name "zerotier")
   (version "1.16.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/zerotier/ZeroTierOne")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "12v7c1v7vh5g3qma4jc7ll95pjcgggqj2hyy4mcvgcbrmz7x2mvc"))))
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

(define-public cloudflared
  (package
    (name "cloudflared")
    (version "2025.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cloudflare/cloudflared")
                    (commit version)))
              (file-name (git-file-name name version))
              ;; TODO: Unbundle vendored dependencies.
              ;; (modules '((guix build utils)))
              ;; (snippet '(delete-file-recursively "vendor"))
              (sha256
               (base32
                "17an3nky4ibfi486wy2gsbv39qcmbsc3yvs3h2w6yxa4cb9knk6v"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.24
           #:install-source? #f
           #:import-path "github.com/cloudflare/cloudflared/cmd/cloudflared"
           #:unpack-path "github.com/cloudflare/cloudflared"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    " -X main.Version=" #$(package-version this-package)
                    " -X github.com/cloudflare/cloudflared/cmd/cloudflared/updater.BuiltForPackageManager=Guix"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'disable-cgo
                 (lambda _
                   (setenv "CGO_ENABLED" "0")))
               (add-after 'install 'install-documentation
                 (lambda _
                   (let ((src "src/github.com/cloudflare/cloudflared/cloudflared_man_template")
                         (dst (string-append #$output "/share/man/man1/cloudflared.1")))
                     (substitute* src
                       (("\\$\\{VERSION\\}") #$(package-version this-package)))
                     (mkdir-p (dirname dst))
                     (copy-file src dst)))))))
    (home-page "https://developers.cloudflare.com/cloudflare-one/connections/connect-apps/")
    (synopsis "Cloudflare Tunnel client")
    (description
     "This package provides the command-line client for Cloudflare Tunnel, a
tunneling daemon that proxies traffic from the Cloudflare network to your
origins.  This daemon sits between Cloudflare network and your origin (e.g. a
webserver).  Cloudflare attracts client requests and sends them to you via
this daemon, without requiring you to poke holes on your firewall --- your
origin can remain as closed as possible.")
    (license license:asl2.0)))
