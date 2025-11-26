(define-module (fox packages games)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages games)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses))

(define-public cataclysm-dda-experimental
  (package
   (inherit cataclysm-dda)
   (name "cataclysm-dda-experimental")
   (version "2025-10-25")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/CleverRaven/Cataclysm-DDA")
           (commit "c30e3e6e1c63cc7132b748ea25a4dcd3f5b99244")))
     (sha256
      (base32 "1gmi2psgchzp62kgbvg6bc38qqksvbngn5n64q02bcp3izpni56a"))
     (file-name (git-file-name name version))))
   (arguments
    (list
     #:make-flags
     #~(list (string-append "PREFIX=" #$output)
             "USE_HOME_DIR=1" "DYNAMIC_LINKING=1" "RELEASE=1" "WARNINGS=-w"
             "LOCALIZE=1" "LANGUAGES=all")
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure))
     #:tests? #f))
   (inputs
    (list ncurses zlib))
   (outputs '("out"))))
