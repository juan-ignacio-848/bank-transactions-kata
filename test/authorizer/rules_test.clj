(ns authorizer.rules-test
  (:require [authorizer.rules :refer :all]
            [clojure.test :refer :all]))

(def transactions [{:merchant "Amazon" :amount 20 :time "2019-10-28T10:00:00.000Z"}
                   {:merchant "Apple" :amount 20 :time "2019-10-28T10:00:00.000Z"}
                   {:merchant "Garmin" :amount 20 :time "2019-10-28T11:05:00.000Z"}
                   {:merchant "Samsung" :amount 20 :time "2019-10-28T11:05:00.000Z"}
                   {:merchant "Samsung" :amount 20 :time "2019-10-28T11:06:00.000Z"}
                   {:merchant "Burger King" :amount 20 :time "2019-10-10T10:00:30.000Z"}
                   {:merchant "Burger King" :amount 20 :time "2019-10-10T10:01:30.000Z"}])

(def inactive-card-account {:account {:active-card false :available-limit 1000} :transactions []})
(def active-card-account {:account {:active-card true :available-limit 1000} :transactions transactions})

(deftest sufficient-limit
  (is (true? (sufficient-limit? {:available-limit 100} 100)))
  (is (false? (sufficient-limit? {:available-limit 100} 200))))

(deftest active-card
  (is (true? (active-card? {:active-card true})))
  (is (false? (active-card? {:active-card false}))))

(deftest high-frequency-small-interval
  (is (true? (high-frequency-small-interval? transactions {:merchant "Disney" :amount 10 :time "2019-10-28T11:05:00.000Z"})))
  (is (false? (high-frequency-small-interval? transactions {:merchant "Samsung" :amount 5 :time "2019-10-28T10:02:01.000Z"}))))

(deftest doubled-transactions
  (is (true? (doubled-transactions? transactions {:merchant "Samsung" :amount 20 :time "2019-10-28T11:05:30.000Z"})))
  (is (false? (doubled-transactions? transactions {:merchant "Garmin" :amount 20 :time "2019-10-28T11:05:00.000Z"})))
  (is (false? (doubled-transactions? transactions {:merchant "Samsung" :amount 25 :time "2019-10-28T11:05:30.000Z"}))))

(deftest simple-transaction-violations
  (is (= (transaction-violations inactive-card-account {:merchant "Disney" :amount 100 :time "2019-10-28T11:05:00.000Z"})
         #{:card-not-active}))
  (is (= (transaction-violations active-card-account {:merchant "Disney" :amount 10000 :time "2019-10-15T11:05:00.000Z"})
         #{:insufficient-limit}))
  (is (= (transaction-violations active-card-account {:merchant "Disney" :amount 10 :time "2019-10-28T11:05:00.000Z"})
         #{:high-frequency-small-interval}))
  (is (= (transaction-violations active-card-account {:merchant "Burger King" :amount 20 :time "2019-10-10T10:01:00.000Z"})
         #{:doubled-transaction})))

(deftest multiple-transaction-violations-can-happen
  (is (= (transaction-violations inactive-card-account {:merchant "Disney" :amount 10000 :time "2019-10-28T11:05:00.000Z"})
         #{:card-not-active :insufficient-limit}))
  (is (= (transaction-violations active-card-account {:merchant "Samsung" :amount 20 :time "2019-10-28T11:05:00.000Z"})
         #{:high-frequency-small-interval :doubled-transaction})))