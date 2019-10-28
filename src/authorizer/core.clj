(ns authorizer.core
  (:require [authorizer.rules :as rules]))

(def violation-codes #{:account-already-initialized
                       :insufficient-limit
                       :card-not-active
                       :high-frequency-small-interval
                       :doubled-transaction})

(def account-info (atom {:account nil :transactions []}))

(defn decrease-available-limit [account amount]
  (update account :available-limit - amount))

(defn pay [account-info tx]
  {:account      (decrease-available-limit (:account account-info) (:amount tx))
   :transactions (conj (:transactions account-info) tx)})

(defn pay! [tx]
  (reset! account-info (pay @account-info tx)))

(defn create-account! [account data]
  (if account
    {:account account :violations #{:account-already-initialized}}
    {:account (:account (swap! account-info assoc :account data)) :violations #{}}))

(defn process-transaction! [tx]
  (let [violations (rules/transaction-violations @account-info tx)]
    (when (empty? violations)
      (pay! tx))
    {:account (:account @account-info) :violations violations}))

(defn authorize! [op]
  (cond
    (:account op) (create-account! (:account @account-info) (:account op))
    (:transaction op) (process-transaction! (:transaction op))))