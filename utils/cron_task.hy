#!/usr/bin/env hy
(import configparser [ConfigParser])
(import sys)

(import requests)


(setv cfg (ConfigParser))
(cfg.read "/root/.config/secret.ini")

(defn sync-anki []
  (let [anki-connect-endpoint (cfg.get "SECRET" "ANKI_CONNECT_ENDPOINT")
        anki-connect-key (cfg.get "SECRET" "ANKI_CONNECT_KEY")
        resp (requests.get f"https://{anki-connect-endpoint}"
                           :json {"key" anki-connect-key "version" 6 "action" "sync"})]
    (when (!= None (get (resp.json) "error"))
      (print "Failed to sync anki!")
      (print (get (resp.json) "error"))
      (sys.exit 1))))


(let [action (get sys.argv 1)]
  (cond
    (= action "sync-anki")
    (sync-anki)
    ))
