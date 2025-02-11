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
  (let ((commit "366d25435229bfa725109a147d6cb2fef8011b3c")
        (url "https://github.com/ohmyzsh/ohmyzsh.git"))
    (package
      (name "oh-my-zsh")
      (version "241125")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url url)
                      (commit commit)))
                (sha256
                 (base32
                  "1w339zy12mnszdbh9gfpym39bhx5klrfpqaqbgh7vc2gw4m6slrc"))))
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
    (version "0.41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/zellij-org/zellij/releases/download/v"
                    version "/zellij-x86_64-unknown-linux-musl.tar.gz"))
              (sha256
               (base32 "0y3cpy20g984jrz8gnc6sqjskfwmfjngd617bksvm9fq2yl23hxi"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
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

(define-public python-hy-1
  (package
    (name "python-hy-1")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/hylang/hy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1msv3027krv223a4dhx6kzp7r7l4q2qg8kq25j4dcf8k5xs73ax3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; This test expects the hy executable to be called 'hy', but in Guix
      ;; it's .hy-real.
      #:test-flags #~(list "-k" "not test_sys_executable")))
    (native-inputs (list python-pytest python-wheel))
    (propagated-inputs (list python-funcparserlib))
    (home-page "https://docs.hylang.org/en/stable/")
    (synopsis "Lisp frontend to Python")
    (description
     "Hy is a dialect of Lisp that's embedded in Python.  Since Hy transforms
its Lisp code into the Python Abstract Syntax Tree, you have the whole world of
Python at your fingertips, in Lisp form.")
    (license expat)))

(define-public hugo-bin
  (package
    (name "hugo-bin")
    (version "0.143.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gohugoio/hugo" "/releases/download/v"
                    version "/hugo_extended_" version "_linux-amd64.tar.gz"))
              (sha256
               (base32
                "0rhhbmn67mkh98wfwzdkchcxj44q22yc69233q7cfd7kqqyvgn18"))))
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
