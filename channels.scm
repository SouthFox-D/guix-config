(append (list (channel
                (name 'fox-channel)
                (url (string-append "file://" (getenv "HOME")
                                    "/.config/guix"))))
        %default-channels)
