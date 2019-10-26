(ns authorizer.core
  (:require [java-time :as time]))

(def violation-codes #{:account-already-initialized
                      :insufficient-limit
                      :card-not-active})

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

; Still not used
(defn time-between [tx-1 tx-2]
  (let [instant-1 (time/instant (:time tx-1))
        instant-2 (time/instant (:time tx-2))]
    (Math/abs (time/time-between instant-1 instant-2 :seconds))))

(defn transaction-violations [account data]
  (cond-> []
          (not (has-enough-money? account (:amount data))) (conj "insufficient-limit")
          (not (has-active-card? account)) (conj "card-not-active")))

(defn process-transaction [tx]
  (let [violations (transaction-violations (:account @account-info) tx)]
    (when (empty? violations)
      (pay! tx))
    (response (:account @account-info) violations)))

(defn process [op]
  (cond
    (:account op) (create-account (:account @account-info) (:account op))
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

  ; Transactions until insufficient limit
  (authorize [
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-03-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-04-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-05-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-06-13T10:00:00.000Z"}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-09-13T10:00:00.000Z"}}
              ])

  ; Card is not active
  (authorize [{:account {:active-card false :available-limit 100}}
              {:transaction {:merchant "Burger King" :amount 20 :time "2019-05-13T10:00:00.000Z"}}]))
