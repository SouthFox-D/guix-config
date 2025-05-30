#!/usr/bin/env hy
(import argparse)
(import datetime [datetime])
(import subprocess)
(import os)


(setv parser (argparse.ArgumentParser))
(parser.add_argument "group" :type str)
(setv args (parser.parse_args))

(defn backup-mastfox []
  (os.chdir "/root/mastodon")
  (let [now (datetime.now)
        now-str (now.isoformat)
        backup-file-name (+ "mstd-backup-" now-str ".dump")]
    (subprocess.run f"docker compose exec db pg_dumpall -U postgres > {backup-file-name}"
                    :shell True
                    :check True)
    (subprocess.run f"rclone copy ./{backup-file-name} backup:/fox-echo -c"
                    :shell True
                    :check True)
    (os.remove backup-file-name)))

(when (= args.group "mastfox") (backup-mastfox))
