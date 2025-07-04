(define-module (fox wow-addons)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))


(define license (@@ (guix licenses) license))

(define* (custom uri #:optional (comment ""))
  "Return a custom license, whose full text can be found
at URI, which may be a file:// URI pointing the package's tree."
  (license "Custom"
           uri
           (string-append
            "This a Custom license.  Check the URI for details."
            comment)))

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

(define-public immersion
  (package
   (name "Immersion")
   (version "1.4.45")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/seblindfors/Immersion.git")
                  (commit version)))
            (sha256
             (base32
              "1y81v0k5m07cgfm91q3b5zs29nzw1xv1js7z3f6izdcvf6m5knqc"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("." "Immersion"))))
   (synopsis "Immersive questing addon for World of Warcraft.")
   (description "Immersive questing addon for World of Warcraft.")
   (home-page "https://github.com/seblindfors/Immersion")
   (license artistic2.0)))

(define-public elvui
  (package
   (name "ElvUI")
   (version "v13.93")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/tukui-org/ElvUI.git")
                  (commit version)))
            (sha256
             (base32
              "10xivwdcnzjk91xsza5f0y2s54ylgzandkh4g5a2m9g1n5fdz7k4"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("." "."
          #:include-regexp ("ElvUI*" )))))
   (synopsis "User Interface replacement AddOn for World of Warcraft")
   (description "User Interface replacement AddOn for World of Warcraft.")
   (home-page "https://tukui.org")
   (license (custom "https://github.com/tukui-org/ElvUI/blob/main/LICENSE.md"))))

(define-public weakauras
  (package
   (name "WeakAuras")
   (version "5.19.12")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/WeakAuras/WeakAuras2.git")
                  (commit version)))
            (sha256
             (base32
              "0jzqkbw5dk1gdh46ahy58zhhxa7i2g5kjkjyvi0vnijdqxmlz97n"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("." "."
          #:include-regexp ("WeakAuras*")))))
   (synopsis " Provides a powerful framework to display customizable
graphics on your screen.")
   (description "World of Warcraft addon that provides a powerful
framework to display customizable graphics on your screen.")
   (home-page "https://weakauras.wtf")
   (license gpl2)))
