(ns authorizer.core
  (:require [time.interval :as time]
            [java-time :refer [instant]])
  (:import (java.time.temporal ChronoUnit)))

;; TODO: Validations should go in another namespace?
;; TODO: Tests - Unit tests, generative tests.
;; TODO: Violation codes are hard-coded, advantages of using this set?
(def violation-codes #{:account-already-initialized
                       :insufficient-limit
                       :card-not-active
                       :high-frequency-small-interval
                       :doubled-transaction})

(defn response [account violations]
  {:account account :violations violations})

(defn create-account-info [account transactions]
  {:account account :transactions transactions})

(def account-info (atom (create-account-info nil [])))

(defn decrease-available-limit [account amount]
  (update account :available-limit - amount))

(defn pay [account-info tx]
  (create-account-info (decrease-available-limit (:account account-info) (:amount tx))
                       (conj (:transactions account-info) tx)))

(defn pay! [tx]
  (reset! account-info (pay @account-info tx)))

(defn create-account! [account data]
  (if account
    (response account #{:account-already-initialized})
    (response (:account (swap! account-info assoc :account data)) #{})))

(defn sufficient-limit? [account amount]
  (>= (:available-limit account)
      amount))

(defn card-active? [account]
  (:active-card account))

(defn transactions-within-interval? [tx-1]
  (fn [tx-2]
    (time/within-interval (instant (:time tx-1))
                                   (time/interval (instant (:time tx-2))
                                                  1 ChronoUnit/MINUTES))))

(defn high-frequency-small-interval? [txs tx]
  (< 2 (count (filter (transactions-within-interval? tx) txs))))

(defn similar-transactions? [tx-1]
  (fn [tx-2]
    (and (= (:amount tx-1) (:amount tx-2))
         (= (:merchant tx-1) (:merchant tx-2)))))

(defn doubled-transactions? [txs tx]
  (< 1 (count (transduce (comp (filter (similar-transactions? tx))
                               (filter (transactions-within-interval? tx)))
                         conj []
                         txs))))

(defn transaction-violations [account-info tx]
  (cond-> #{}
          (not (sufficient-limit? (:account account-info) (:amount tx))) (conj :insufficient-limit)
          (not (card-active? (:account account-info))) (conj :card-not-active)
          (high-frequency-small-interval? (:transactions account-info) tx) (conj :high-frequency-small-interval)
          (doubled-transactions? (:transactions account-info) tx) (conj :doubled-transaction)))

(defn process-transaction! [tx]
  (let [violations (transaction-violations @account-info tx)]
    (when (empty? violations)
      (pay! tx))
    (response (:account @account-info) violations)))

(defn authorize! [op]
  (cond
    (:account op) (create-account! (:account @account-info) (:account op))
    (:transaction op) (process-transaction! (:transaction op))))