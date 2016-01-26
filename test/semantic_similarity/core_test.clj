(ns semantic-similarity.core-test
  (:require [clojure.test :refer :all]
            [semantic-similarity.core :refer :all]))

(deftest get-only-alpha-test
  (is (= (get-only-alpha "foo123")
         "foo")))

