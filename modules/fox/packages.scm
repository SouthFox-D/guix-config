(define-module (fox packages)
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
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu home services)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu services))

(define-public oh-my-zsh
  (let ((commit "f97e871c33da57d20e1b0411da6b1674126192a9")
        (url "https://github.com/ohmyzsh/ohmyzsh.git"))
    (package
      (name "oh-my-zsh")
      (version "250319")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url url)
                      (commit commit)))
                (sha256
                 (base32
                  "14wcch5swvq2xqs3zs5hszrbi0rp8zvlj4wcz0rz45b61s9kss1g"))))
      (build-system copy-build-system)
      (synopsis "framework for managing your zsh configuration")
      (description "🙃A delightful community-driven (with 2,400+ contributors)
framework for managing your zsh configuration. Includes 300+ optional plugins
(rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes
to spice up your morning, and an auto-update tool that makes it easy to keep up
with the latest updates from the community.")
      (home-page "https://ohmyz.sh/")
      (license expat))))

(define-public zellij
  (package
    (name "zellij")
    (version "0.42.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/zellij-org/zellij/releases/download/v"
                    version "/zellij-x86_64-unknown-linux-musl.tar.gz"))
              (sha256
               (base32 "0s16924xx02d788gk5qbh13fw45d8zkmj2wrz5kw4d1ivnqj237b"))))
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
    (home-page "https://github.com/zellij-org/zellij")
    (synopsis "terminal workspace with batteries included")
    (description "Terminal workspace with batteries included.")
    (license expat)))

(define-public hugo-bin
  (package
    (name "hugo-bin")
    (version "0.145.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gohugoio/hugo" "/releases/download/v"
                    version "/hugo_extended_" version "_linux-amd64.tar.gz"))
              (sha256
               (base32
                "1r2alw2a3acs99dx89p886p3qbwpds6kpgz510jjiym8dna6hx3w"))))
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
    (version "0.61.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fatedier/frp" "/releases/download/v"
                    version "/frp_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1sh5ib9ffzq1glqwgc2hrk4rid537pb4dsff1kzbb3gq9fyysf27"))))
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
    (license asl2.0)))

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
    (license asl2.0)))
