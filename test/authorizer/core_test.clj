(ns authorizer.core-test
  (:require [clojure.test :refer :all]
            [authorizer.core :refer :all]))

;; TODO: CHECK IF SUPER BAD PRACTICE.
(defn start-over [f]
  (reset! account-info (create-account-info nil []))
  (f))

(use-fixtures :each start-over)

(deftest create-account
  (is (= (authorize! {:account {:active-card true :available-limit 100}})
         {:account {:active-card true :available-limit 100} :violations #{}})))

(deftest account-should-not-be-recreated
  (authorize! {:account {:active-card true :available-limit 100}})
  (is (= (authorize! {:account {:active-card true :available-limit 100}})
         {:account {:active-card true :available-limit 100} :violations #{:account-already-initialized}})))

(deftest decrease-available-limit-if-there-are-no-violations
  (authorize! {:account {:active-card true :available-limit 100}})
  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 80}, :violations #{}})))

(deftest card-not-active-violation
  (authorize! {:account {:active-card false :available-limit 100}})
  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card false, :available-limit 100}, :violations #{:card-not-active}})))

(deftest insufficient-limit-violation
  (authorize! {:account {:active-card true :available-limit 100}})
  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 200 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 100}, :violations #{:insufficient-limit}})))

(deftest high-frequency-small-interval-violation
  (authorize! {:account {:active-card true :available-limit 100}})
  (authorize! {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
  (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
  (authorize! {:transaction {:merchant "Apple" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 40}, :violations #{:high-frequency-small-interval}})))

(deftest doubled-transactions-violation
  (authorize! {:account {:active-card true :available-limit 100}})
  (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
  (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 60}, :violations #{:doubled-transaction}})))

(deftest many-violations
  (is (= (authorize! {:account {:active-card true :available-limit 100}})
         {:account {:active-card true, :available-limit 100}, :violations #{}}))

  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 80}, :violations #{}}))

  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 60}, :violations #{}}))

  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 40}, :violations #{}}))

  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 40}, :violations #{:doubled-transaction :high-frequency-small-interval}}))

  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 200 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 40}, :violations #{:insufficient-limit :high-frequency-small-interval}}))

  (is (= (authorize! {:transaction {:merchant "Burger King" :amount 40 :time "2019-02-12T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 0}, :violations #{}}))

  (is (= (authorize! {:transaction {:merchant "Amazon" :amount 20 :time "2019-02-13T10:00:00.000Z"}})
         {:account {:active-card true, :available-limit 0}, :violations #{:insufficient-limit :doubled-transaction :high-frequency-small-interval}})))
