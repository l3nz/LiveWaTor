(ns livewator.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is testing run-tests]]

            ))

(enable-console-print!)

(println "This text is printed from src/livewator/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload


(defn buildrow
  "Crea una vettore riga di n elementi ripetuti
   Serve per inizializzare il campo da gioco."
  [n-items value]
  (vec (repeat n-items value)))


(defn empty-playfield
  "Crea un campo da gioco vuoto; matrice w*h di elementi nil"
  [w h]
  (buildrow h (buildrow w nil))
)

(def PLAYFIELD-WIDTH 30)
(def PLAYFIELD-HEIGHT 20)

(defonce app-state (atom {:pf (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT)}))


; ==========================================================
; Animals
;
; {:tipo :PESCE }
;
; {:tipo :SQUALO :energia 3}
;

(def MOSSE {
   :SU   [-1  0]
   :GIU  [ 1  0]
   :SX   [ 0 -1]
   :DX   [ 0  1]
   :SUSX [-1 -1]
   :GSX  [ 1 -1]
   :SUDX [-1  1]
   :GDX  [ 1  1]
})

(defn somma-circolare [pos offset massimo]
  (let [p (+ pos offset)
        m (dec massimo)]
    (cond
      (< p 0) m
      (> p m) 0
      :else p)))


(defn nuovopunto [[r c] direzione]
  (let [[o-r o-c] (direzione MOSSE)]
    [(somma-circolare r o-r PLAYFIELD-HEIGHT)
     (somma-circolare c o-c PLAYFIELD-WIDTH)
     ]))




(defn mossa-casuale []
  (rand-nth [:SU :GIU :SX :DX :SUSX :GSX :SUDX :GDX])
  ;:DX
  )

; defn animali
(defn crea-nuovo-pesce []
  {:tipo    :PESCE
   :energia 0
   :turns   0})

(defn crea-nuovo-squalo []
  {:tipo    :SQUALO
   :energia 0
   :turns   0})

(defn imposta-campo-di-gioco [celle]
  (swap! app-state assoc-in [:pf] celle) )

(defn setta-energia [animale value]
  (assoc-in animale [:energia] value))

(defn incrementa-energia [animale]
  (update-in animale [:energia] inc))

(defn cella-vuota? [celle rc]
  (if (nil? (get-in celle rc))
    true
    false))

(defn imposta-animale [celle [r c] animale]
  (if (nil? animale)
    (assoc-in celle [r c] nil)
    (let [animale-decrementa (assoc-in animale [:turns] 0)]
      (assoc-in celle [r c] animale-decrementa)
    )))


(defmulti avanza-animale
  (fn [celle [r c] animale] (:tipo animale)))

(defmethod avanza-animale nil
  [celle [r c] animale]
  celle)

(defn figlia-pesce
  "Restituisce il pesce padre e l'eventuale figlio, oppure nil"
  [padre]
  (if (> 5 (:energia padre))
    [padre nil]
    [(setta-energia padre 0) (crea-nuovo-pesce)]))


(defmethod avanza-animale :PESCE
  [celle [r c] animale]
  (let [pesce (incrementa-energia animale)
        nuovacella (nuovopunto [r c] (mossa-casuale))
        ;x (println (str "Pesce " r "x" c "-" animale "-nc" nuovacella))
        ]
    (if (cella-vuota? celle nuovacella)
      (let [[nuovo vecchio] (figlia-pesce pesce)]
        (-> celle
          (imposta-animale nuovacella nuovo)
          (imposta-animale [r c] vecchio)))
       celle
      )))

(defmethod avanza-animale :SQUALO
  [celle [r c] animale]
  celle)

(defn avanza-gioco-cella [celle rc]
  (doall
     (println "Celle " rc " - " celle)
     (let [animale (get-in celle rc)
           turni   (:turns animale)]
       (if (pos? turni)
         (avanza-animale celle rc animale)
         celle
         ))))

(defn aggiunge-1-turno [celle [r c]]
  (let [animale (get-in celle [r c])]
    (if-not (nil? animale)
      (assoc-in celle [r c :turns] 1)
      celle)))

(defn coordinate-gioco []
  (for [r (range PLAYFIELD-HEIGHT)
        c (range PLAYFIELD-WIDTH)]
    [r c]))


(defn avanza-gioco [celle]
  (let [celle-con-turni (reduce aggiunge-1-turno celle (coordinate-gioco))]


  (reduce avanza-gioco-cella celle-con-turni (coordinate-gioco) )))





;
; Plots the playfield, it is a table of size w*h
;

(defmulti plot-cell (fn [data] (:tipo data)))

(defmethod plot-cell :PESCE
  [data]
  [:td (str "P" (:energia data))] )

(defmethod plot-cell :SQUALO
  [data]
  [:td "@"] )

(defmethod plot-cell nil
  [data]
  [:td ""] )


;(defn plot-cell [data]
;  [:td (str "x" data)]
;  )

(defn plot-row
  "[:tr
     [:td 'Matthew']
     [:td '26']]"
  [nrow rowdata]

  ^{:key (str "r-" nrow)} [:tr
    [:td (str nrow)]
    (map plot-cell rowdata)
    ]
  )



(defn plot-playfield []
  [:div

  [:table.table.table-striped.table-bordered
   {:cell-spacing "0" }

   [:thead>tr
     [:td {:width "20px"} "-" ]
     (map #(vec [:th {:width "20px"} (str %)]) (range PLAYFIELD-WIDTH))
    ]

   [:tbody

    (let [table (:pf @app-state)]
      (for [row (range PLAYFIELD-HEIGHT)]
        (plot-row row (get table row))))

    ]]

    [:input {:type "button" :value (str "Avanza #" (:turn @app-state))
            :on-click
             #(doall
                (imposta-campo-di-gioco
                  (avanza-gioco (:pf @app-state) ))
                (swap! app-state update-in [:turn] inc))  }]

    [:input {:type "button" :value "Reset!"
            :on-click
             #(doall
                (imposta-campo-di-gioco (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT))
                (swap! app-state assoc-in [:turn] 0))     }]


    [:input {:type "button" :value "Pesce 1 1"
            :on-click
             #(imposta-campo-di-gioco
                (imposta-animale (:pf @app-state) [1 1] (crea-nuovo-pesce))) }]

     [:br ]
     ;[:pre (str @app-state)]



     ]


  )


(reagent/render-component [plot-playfield]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
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
    (is (= (nuovopunto [1 1] :XXX)  [1 1]))
))



;(enable-console-print!)
;(run-tests)











