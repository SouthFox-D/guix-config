(define-module (fox packages wow-addons)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (fox packages licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public consoleport
  (package
   (name "ConsolePort")
   (version "3.1.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/seblindfors/ConsolePort/releases/download/"
                  version "/ConsolePort-" version ".zip"))
            (sha256
             (base32
              "06n3firk2z2g9ihwybimmfxnpb84gbj6nzx42f4v1gm8r7wqxhnx"))))
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
   (license license:gpl2)))

(define-public immersion
  (package
   (name "Immersion")
   (version "1.4.48")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/seblindfors/Immersion.git")
                  (commit version)))
            (sha256
             (base32
              "0bmmqyvyiv4l65jzxwmrvfs0qb38h91m1jkrdnq6igypvia5al61"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("." "Immersion"))))
   (synopsis "Immersive questing addon for World of Warcraft.")
   (description "Immersive questing addon for World of Warcraft.")
   (home-page "https://github.com/seblindfors/Immersion")
   (license license:artistic2.0)))

(define-public weakauras
  (package
   (name "WeakAuras")
   (version "5.20.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/WeakAuras/WeakAuras2/releases/download/"
                  version "/WeakAuras-" version ".zip"))
            (sha256
             (base32
              "0sv1sw6wfimpl8q6g5f501aa8vqq3rd32x1dnz0pclb0gxyr53k5"))))
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
   (license license:gpl2)))

(define-public cell
  (package
   (name "Cell")
   (version "r269-release")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/enderneko/Cell/releases/download/"
                  version "/Cell-" version ".zip"))
            (sha256
             (base32
              "1n8m0nilzd5rd0bwa7xj50zdziw696xfx5j5xmms3kd76xgdf0lh"))))
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

(define-public deadly-boss-mods
  (package
   (name "DeadlyBossMods")
   (version "12.0.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/DeadlyBossMods/DeadlyBossMods/releases/download/"
                  version "/DBM-Core-" version ".zip"))
            (sha256
             (base32
              "00br016i6jrav8lxhgp6dq2d4g0y9wzsymwydpzzfa9v7y91fnga"))))
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
   (synopsis "The ultimate encounter helper to give you fight info that's easy
to process at a glance.")
   (description "DBM aims to focus on what's happening to you, and what YOU
need to do about it.")
   (home-page "https://www.curseforge.com/wow/addons/deadly-boss-mods")
   (license (custom "https://github.com/DeadlyBossMods/DeadlyBossMods/blob/master/LICENSE"))))
