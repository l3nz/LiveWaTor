(ns livewator.model-test
  (:use clojure.test)
  (:use livewator.model)

  )

; ====================================================
; TESTS
; (in-ns 'livewator.core)
; (run-tests)


(deftest test-somma-circolare
  (is (= 1 (somma-circolare 0 1 4)))
  (is (= 2 (somma-circolare 1 1 4)))
  (is (= 3 (somma-circolare 2 1 4)))
  (is (= 0 (somma-circolare 3 1 4)))
  (is (= 1 (somma-circolare 0 1 4)))
  (is (= 2 (somma-circolare 1 1 4)))

  (is (= 3 (somma-circolare 0 -1 4)))
  (is (= 2 (somma-circolare 3 -1 4)))
  (is (= 1 (somma-circolare 2 -1 4)))
  (is (= 0 (somma-circolare 1 -1 4)))
  (is (= 3 (somma-circolare 0 -1 4)))
  (is (= 2 (somma-circolare 3 -1 4)))
  (is (= 1 (somma-circolare 2 -1 4)))
  (is (= 0 (somma-circolare 1 -1 4)))
)

(deftest test-nuovopunto
  (let [maxrow (dec PLAYFIELD-HEIGHT)]
    (is (= (nuovopunto [1 1] :SU)  [0 1]))
    (is (= (nuovopunto [1 1] :GIU) [2 1]))
    (is (= (nuovopunto [1 1] :SX)  [1 0]))
    (is (= (nuovopunto [1 1] :DX)  [1 2]))

    (is (= (nuovopunto [0 0] :SU)  [maxrow 0]))
    (is (= (nuovopunto [maxrow 0] :GIU)  [0 0]))
    ;;(is (= (nuovopunto [1 1] :XXX)  [1 1]))
))



;(enable-console-print!)
;(run-tests)
