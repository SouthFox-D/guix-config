#!/usr/bin/env hy
(import subprocess)
(import time)
(import os)

(defn run-cmd [cmd [user "root"] [check True]]
  (print user "--" "Run cmd: " cmd)
  (when (!= user "root")
    (subprocess.run f"sudo -u {user} sh -c \"{cmd}\"" :shell True :check check)
    (return))
  (subprocess.run cmd :shell True :check check))

(defn get-cut []
  (let [sudo-user (os.environ.get "SUDO_USER")
        guix-workdir f"/home/{sudo-user}/.config/guix"
        guix-substitute "--substitute-urls=\"https://guix.southfox.me\""
        need-reload-hyprland? (os.path.isfile "/usr/bin/hyprctl")
        need-upgrade-doomemacs? (os.path.isfile (+ "/home/" sudo-user "/.doom.d/config.el"))]
    (run-cmd f"git -C {guix-workdir} pull" sudo-user)
    (run-cmd f"guix repl -L {guix-workdir}/modules  {guix-workdir}/arch.scm")
    (run-cmd f"guix pull {guix-substitute} -v 4 && systemctl restart guix-daemon.service")
    (run-cmd f"guix pull {guix-substitute} -v 4" sudo-user)
    (run-cmd f"cd {guix-workdir} && guix home reconfigure home-configuration.scm -L modules {guix-substitute} -v 4" sudo-user)
    (when need-reload-hyprland?
      (run-cmd f"hyprctl reload -i 0" sudo-user))
    (when need-upgrade-doomemacs?
      (run-cmd f"git -C /home/{sudo-user}/.doom.d pull" sudo-user False)
      (run-cmd f"DOOMGITCONFIG=~/.gitconfig /home/{sudo-user}/.emacs.d/bin/doom upgrade" sudo-user))))

(get-cut)
