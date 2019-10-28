(ns authorizer.core
  (:require [authorizer.rules :as rules]))

(def account-info (atom {:account nil :transactions []}))

(defn pay [account-info tx]
  {:account      (update (:account account-info) :available-limit - (:amount tx))
   :transactions (conj (:transactions account-info) tx)})

(defn process-transaction! [tx]
  (let [violations (rules/transaction-violations @account-info tx)]
    (when (empty? violations)
      (reset! account-info (pay @account-info tx)))
    {:account (:account @account-info) :violations violations}))

(defn create-account! [data]
  (let [violations (rules/account-violations @account-info)]
    (when (empty? violations)
      (swap! account-info assoc :account data))
    {:account (:account @account-info) :violations violations}))

(defn authorize! [op]
  (cond
    (:account op) (create-account! (:account op))
    (:transaction op) (process-transaction! (:transaction op))))