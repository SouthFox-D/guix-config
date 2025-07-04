(define-module (fox wow-addons)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
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
            (method url-fetch)
            (uri (string-append
                  "https://github.com/seblindfors/ConsolePort/releases/download/"
                  version "/ConsolePort-" version ".zip"))
            (sha256
             (base32
              "0pda9wr40b0rk66s2vxynf60pmgwab3jrwyy13nzhxv27irxi47l"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases
        %standard-phases
        (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "unzip" source))))))
   (native-inputs (list unzip))
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

(define-public weakauras
  (package
   (name "WeakAuras")
   (version "5.19.12")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/WeakAuras/WeakAuras2/releases/download/"
                  version "/WeakAuras-" version ".zip"))
            (sha256
             (base32
              "05dd9gc7w2mz7ksgy1wjbfyx5y4kxjrzz98whmmk2k6yzm1vpc5s"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases
        %standard-phases
        (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "unzip" source))))))
   (native-inputs (list unzip))
   (synopsis " Provides a powerful framework to display customizable
graphics on your screen.")
   (description "World of Warcraft addon that provides a powerful
framework to display customizable graphics on your screen.")
   (home-page "https://weakauras.wtf")
   (license gpl2)))

(define-public cell
  (package
   (name "Cell")
   (version "r253-release")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/enderneko/Cell/releases/download/"
                  version "/Cell-" version ".zip"))
            (sha256
             (base32
              "0rl74linnlspyzsld49d3qm7sf1ibiyzbca9ih802nar3rl7wnr5"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases
        %standard-phases
        (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "unzip" source))))))
   (native-inputs (list unzip))
   (synopsis "A World of Warcraft raid frame addon")
   (description "Cell is a nice raid frame addon inspired by several great
addons, such as CompactRaid, Grid2, Aptechka and VuhDo. With a more
human-friendly interface, Cell can provide a better user experience, better
than ever.")
   (home-page "https://github.com/enderneko/Cell")
   (license (custom "https://github.com/enderneko/Cell/blob/master/LICENSE.txt"))))
