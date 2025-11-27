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
   (version "2025-11-27")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/CleverRaven/Cataclysm-DDA")
           (commit "4352393527eabc7744bc82239aa8a62e80ad2bc8")))
     (sha256
      (base32 "1xbjfrkf5sl4gj2f8yzv23dxl35igz6q7ywn1pdcscm69w4xvl8h"))
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
