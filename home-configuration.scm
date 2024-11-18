;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages base)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells))

(load "packages.scm")

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (append  (specifications->packages
                     (list
                      "guile-hoot"
                      "guile-next"
                      "zsh"
                      "zsh-autosuggestions"
                      "zsh-syntax-highlighting"
                      "fzf"))
                    (list
                     zellij)))

 (services
  (list
   (service oh-my-zsh-service-type)
   (service home-emacs-service-type)
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc (list (local-file "files/zshrc"))))))))
