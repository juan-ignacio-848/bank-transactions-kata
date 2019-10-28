(ns authorizer.rules
  (:require [time.interval :as time]
            [java-time :refer [instant]])
  (:import (java.time.temporal ChronoUnit)))

(defn sufficient-limit? [account amount]
  (>= (:available-limit account) amount))

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

(defn account-violations [account-info]
  (if (:account account-info)
    #{:account-already-initialized}
    #{}))