#!/usr/bin/env hy
(import argparse)
(import configparser [ConfigParser])
(import sys)

(import requests)


(setv cfg (ConfigParser))
(cfg.read "/root/.config/cf-kv.ini")

(setv parser (argparse.ArgumentParser))
(setv subparser (parser.add_subparsers :dest "command"))
(setv get-parser (subparser.add_parser "get"))
(get-parser.add_argument "env_key" :type str)
(setv put-parser (subparser.add_parser "put"))
(put-parser.add_argument "env_key" :type str)
(put-parser.add_argument "env_value" :type str)
(setv args (parser.parse_args))

(defn get-env [env-key]
  (let [env-ns-id (cfg.get "INFRA" "CF_KV_NAMESPACE_ID")
        cf-account-id (cfg.get "INFRA" "CF_ACCOUNT_ID")
        cf-kv-api-token (cfg.get "INFRA" "CF_KV_API_TOKEN")
        resp (requests.get f"https://api.cloudflare.com/client/v4/accounts/{cf-account-id}/storage/kv/namespaces/{env-ns-id}/values/{env-key}"
                          :headers {"Authorization" f"Bearer {cf-kv-api-token}"})]
    (when (!= 200 resp.status-code)
      (print "Failed to get env!")
      (sys.exit 1))
    (print (get (resp.json) env-key))))

(defn put-env [env-key env-value]
  (let [env-ns-id (cfg.get "INFRA" "CF_KV_NAMESPACE_ID")
        cf-account-id (cfg.get "INFRA" "CF_ACCOUNT_ID")
        cf-kv-api-token (cfg.get "INFRA" "CF_KV_API_TOKEN")
        resp (requests.put f"https://api.cloudflare.com/client/v4/accounts/{cf-account-id}/storage/kv/namespaces/{env-ns-id}/values/{env-key}"
                           :headers {"Authorization" f"Bearer {cf-kv-api-token}"}
                           :json {env-key env-value})]
    (when (!= 200 resp.status-code)
      (sys.exit 1))))

(when (= args.command "get") (get-env args.env-key))
(when (= args.command "put") (put-env args.env-key args.env-value))
