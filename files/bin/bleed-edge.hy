#!/usr/bin/env hy
(import subprocess)
(import time)
(import os)

(defn run-cmd [cmd [user "root"]]
  (print user "--" "Run cmd: " cmd)
  (when (!= user "root")
    (subprocess.run (+ "sudo -u " user " " cmd) :shell True :check True)
    (return))
  (subprocess.run cmd :shell True :check True))

(defn get-cut []
  (let [sudo-user (os.environ.get "SUDO_USER")
        guix-workdir f"/home/{sudo-user}/.config/guix"]
    (run-cmd f"git -C {guix-workdir} pull" sudo-user)
    (run-cmd f"guix repl -L {guix-workdir}/modules  {guix-workdir}/arch.scm")
    (run-cmd "guix pull -v 4 && systemctl restart guix-daemon.service")
    (run-cmd "guix pull -v 4" sudo-user)
    (run-cmd f"DOOMGITCONFIG=~/.gitconfig /home/{sudo-user}/.emacs.d/bin/doom upgrade" sudo-user)))

(get-cut)
