(require '[clojure.contrib.test-is :as test-is])

(deftest module-tick-reactor
  (test-is/are
   (= _1 (:energy (module-tick {:type :reactor :energy _2 :max-energy 42 :output 10})))
   42 42
   11 1))