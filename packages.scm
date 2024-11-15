(use-modules (guix licenses)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix utils)
             (gnu packages)
             (guix build-system copy)
             (gnu services))

(define-public oh-my-zsh
  (let ((commit "ca5471fe496f00007727fd26db762d19519c2e8f")
        (url "https://github.com/ohmyzsh/ohmyzsh.git"))
    (package
      (name "oh-my-zsh")
      (version "231115")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url url)
                      (commit commit)))
                (sha256
                 (base32
                  "08a9x04jgli4xqclaqzyvrzb74kngnf2vg4q6wyqanrpskgbp3mc"))))
      (build-system copy-build-system)
      (synopsis "Oh My Zsh is an open source, community-driven framework for managing your zsh configuration.")
      (description "🙃A delightful community-driven (with 2,400+ contributors) framework for managing
your zsh configuration. Includes 300+ optional plugins
(rails, git, macOS, hub, docker, homebrew, node, php, python, etc), 140+ themes to spice up
your morning, and an auto-update tool that makes it easy to keep up with the latest updates from the community. ")
      (home-page "https://ohmyz.sh/")
      (license expat))))

(define oh-my-zsh-service-type
  (service-type (name 'oh-my-zsh)
                (description "oh my zsh")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        (lambda (_) (list `(".oh-my-zsh" ,oh-my-zsh))))))))
