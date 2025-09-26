#!/usr/bin/env hy
(import re)
(import json)
(import datetime)
(import random)
(import requests)


(defn get-clock-data [time-str]
   (.json
     (requests.get
       f"https://literature-clock.jenevoldsen.com/times/{time-str}.json")))

(defn insert-linebreak [text interval]
  (let [text-list []
        last-post 0
        split-pos 0]
    (while (> (len text) last-post )
      (setv split-pos (+ interval last-post))
      (text-list.append (cut text last-post split-pos))
      (setv last-post split-pos))
    (.join "\n" text-list)))

(try
  (let [
        now-time (datetime.datetime.now)
        now-time-str (now-time.strftime "%H_%M")
        current-literature-clock-data (get-clock-data now-time-str)
        current-literature (get current-literature-clock-data
                                (random.randint
                                  0
                                  (- (len current-literature-clock-data) 1)))
        current-literature-quote-time (get current-literature "quote_time_case")
        literature-text (.replace (+ (get current-literature "quote_first")
                                     f"<span font-weight='bold'>{current-literature-quote-time}</span>"
                                     (get current-literature "quote_last")
                                     )
                                  "<br/>" "\n"
                                  )]

    (print (+ (.strip (insert-linebreak literature-text 100)) "\n\n\n" (* 80 " ")
              "--"
              (get current-literature "title")
              ","
              (get current-literature "author")
              )))
  (except [KeyError requests.ConnectionError]
    (print (+ "Unknow Unknow " f"<span font-weight='bold'>{(.strftime (datetime.datetime.now) "%H:%M")}</span>"))))
