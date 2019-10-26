(ns authorizer.core
  (:require [java-time :as time]))

(def violation-codes {:account-already-initialized "account-already-initialized"
                      :insufficient-limit "insufficient-limit"
                      :card-not-active "card-not-active"})

(defn account-from [data]
  data)

(defn response [account violations]
  {:account account :violations violations})

(def account (atom nil))

(defn pay [account amount]
  (update account :available-limit - amount))

(defn pay! [amount]
  ;(swap! account update :available-limit - amount)
  (reset! account (pay @account amount)))

(defn create-account! [data]
  (reset! account (account-from data)))

(defn create-account [account data]
  (if account
    (response account ["account-already-initialized"])
    (response (create-account! data) [])))

(defn has-enough-money? [account amount]
  (>= (:available-limit account)
      amount))

(defn has-active-card? [account]
  (:active-card account))

(defn time-between [tx-1 tx-2]
  (let [instant-1 (time/instant (:time tx-1))
        instant-2 (time/instant (:time tx-2))]
    (Math/abs (time/time-between instant-1 instant-2 :seconds))))

(defn transaction-violations [account data]
  (cond-> []
          (not (has-enough-money? account (:amount data))) (conj "insufficient-limit")
          (not (has-active-card? account)) (conj "card-not-active")))

(defn process-transaction [data]
  (let [violations (transaction-violations @account data)]
    (when (empty? violations)
      (pay! (:amount data)))
    (response @account violations)))

(defn process [op]
  (cond
    (:account op) (create-account @account (:account op))
    (:transaction op) (process-transaction (:transaction op))))

(defn authorize [ops]
  (map process ops))

(comment

  ; Start over
  (reset! account nil)

  ; Check account
  @account

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