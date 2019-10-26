(ns authorizer.core
  (:require [time.interval :as time])
  (:import (java.time.temporal ChronoUnit)))

;; TODO: Validations should go in another namespace?

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
    (response account ["account-already-initialized"])
    (response (:account (swap! account-info assoc :account data)) [])))

(defn has-enough-money? [account amount]
  (>= (:available-limit account)
      amount))

(defn has-active-card? [account]
  (:active-card account))

(defn transactions-within-interval [txs tx]
  (filter (fn [{:keys [time]}]
            (time/within-interval (time/instant time)
                                  (time/interval (time/instant (:time tx))
                                                 1 ChronoUnit/MINUTES)))
          txs))

(defn high-frequency-small-interval? [txs tx]
  (> (count (transactions-within-interval txs tx)) 3))

(defn transaction-violations [account-info tx]
  (cond-> []
          (not (has-enough-money? (:account account-info) (:amount tx))) (conj "insufficient-limit")
          (not (has-active-card? (:account account-info))) (conj "card-not-active")
          (high-frequency-small-interval? (:transactions account-info) tx) (conj "high-frequency-small-interval")))

(defn process-transaction [tx]
  (let [violations (transaction-violations @account-info tx)]
    (when (empty? violations)
      (pay! tx))
    (response (:account @account-info) violations)))

(defn process [op]
  (cond
    (:account op) (create-account! (:account @account-info) (:account op))
    (:transaction op) (process-transaction (:transaction op))))

(defn authorize [ops]
  (map process ops))

(comment

  ; Start over
  (reset! account-info (create-account-info nil []))

  ; Check account
  @account-info

  ; Create an account
  (authorize [{:account {:active-card true :available-limit 100}}])
  (authorize [{:account {:active-card false :available-limit 100}}])

  ; Account is already initialized
  (authorize [{:account {:active-card true :available-limit 100}}
              {:account {:active-card true :available-limit 200}}])

  ; Transaction
  (authorize [{:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}}])
  (authorize [{:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:30.000Z"}}])
  (authorize [{:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:01:30.000Z"}}])
  (authorize [{:transaction {:merchant "Burger King" :amount 1 :time "2019-02-13T10:01:00.000Z"}}])
  (authorize [{:transaction {:merchant "Burger King" :amount 1 :time "2019-02-13T10:01:00.000Z"}}])

  ; Transactions until insufficient limit
  (authorize [
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-03-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-04-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-05-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-06-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-09-13T10:00:00.000Z"}}
              ])

  ; Transactions until insufficient limit
  (authorize [
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:02:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"}}
              ])


  (high-frequency-small-interval? [
                                   {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"}
                                   {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"}
                                   {:merchant "Burger King" :amount 20 :time "2019-03-13T10:04:00.000Z"}

                                   ] {:merchant "Burger King" :amount 20 :time "2019-03-13T10:02:00.000Z"})

  ; Card is not active
  (authorize [{:account {:active-card false :available-limit 100}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-05-13T10:00:00.000Z"}}]))
