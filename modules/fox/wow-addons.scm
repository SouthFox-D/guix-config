(define-module (fox wow-addons)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public consoleport
  (package
    (name "ConsolePort")
    (version "2.9.68")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/seblindfors/ConsolePort.git")
                    (commit version)))
              (sha256
               (base32
                "0xcaibb2hfz18ac7qkpf8yg1dpsamn2qxcy7rl896r26yq7g64bh"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "."
           #:include-regexp ("ConsolePort*" )))))
    (synopsis "Game Controller Addon for World of Warcraft")
    (description "ConsolePort is an interface add-on for World of Warcraft
that will give you a handful of nifty features in order to let you play
the game on a controller - without inconvenience.")
    (home-page "https://github.com/seblindfors/ConsolePort")
    (license gpl2)))
