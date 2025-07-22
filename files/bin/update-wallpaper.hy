#!/usr/bin/env hy
(import json)
(import requests)
(import subprocess)
(import argparse)
(import re)
(import pathlib [Path])
(import datetime [date])


(let [wallpaper-path (.expanduser (Path "~/Pictures/Wallpaper/Bing"))
      bing-wallpaper-path (/ wallpaper-path (.isoformat (date.today)))
      current-wallpaper-symlink-path (/ wallpaper-path "current")]
  (when (not (wallpaper-path.exists))
    (wallpaper-path.mkdir :parents True))
  (when (not (.exists bing-wallpaper-path))
    (let [tmb-wallpaper-url (get
                              (re.findall r"\"og\:image\"\ .*?(https://www.bing.com.*?)\&"
                                          (str (. (requests.get "https://bing.com" ) content ))) 0)
          wallpaper-url (tmb-wallpaper-url.replace "tmb" "UHD")
          r (requests.get wallpaper-url)]
      (r.raise_for_status)
      (with [f (open bing-wallpaper-path "wb")]
        (f.write r.content))
      (current-wallpaper-symlink-path.unlink :missing_ok True)
      (current-wallpaper-symlink-path.symlink_to bing-wallpaper-path)))
  (subprocess.run f"hyprctl -i 0 hyprpaper reload ,\"{bing-wallpaper-path}\""
                  :shell True))
