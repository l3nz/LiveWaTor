(ns livewator.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is testing run-tests]]

            ))

(enable-console-print!)

;;(println "This text is printed from src/livewator/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload



(defn buildrow
  "Crea una vettore riga di n elementi ripetuti
   Serve per inizializzare il campo da gioco."
  [n-items value]
  (vec (repeat n-items value)))

(defn empty-playfield
  "Crea un campo da gioco vuoto; matrice w*h di elementi nil"
  [w h]
  (buildrow h (buildrow w nil)))

(def TIPI-PESCI [:PESCE1 :PESCE2 :PESCE3])
(def TIPI-SQUALI [:SQUALO1 :SQUALO2 :SQUALO3])

(def PLAYFIELD-WIDTH 30)
(def PLAYFIELD-HEIGHT 20)


(defonce app-state (atom
      {:pf (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT)
       :turn 0
       :n_pesci '(0)
       :n_squali '(0)
       :loop_ms  '(0)
       }))


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

(def DIREZIONI (keys MOSSE))

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
  (rand-nth DIREZIONI))

; defn animali
(defn crea-nuovo-pesce []
  {:tipo    :PESCE
   :colore  (rand-nth TIPI-PESCI)
   :energia 0
   :turns   0})

(defn crea-nuovo-squalo []
  {:tipo    :SQUALO
   :colore  (rand-nth TIPI-SQUALI)
   :energia 10
   :turns   0})

(defn imposta-campo-di-gioco [celle]
  (swap! app-state assoc-in [:pf] celle) )

(defn setta-energia [animale value]
  (assoc-in animale [:energia] value))

(defn incrementa-energia [animale]
  (update-in animale [:energia] inc))

(defn decrementa-energia [animale]
  (update-in animale [:energia] dec))


(defn cella-vuota? [celle rc]
  (if (nil? (get-in celle rc))
    true
    false))

(defn cella-con-pesce? [celle rc]
  (if (= :PESCE (:tipo (get-in celle rc)))
    rc
    false))



(defn imposta-animale [celle [r c] animale]
  (if (nil? animale)
    (assoc-in celle [r c] nil)
    (let [animale-decrementa (assoc-in animale [:turns] 0)]
      (assoc-in celle [r c] animale-decrementa)
    )))

(defn muovi-animale [celle rc1 animale1 rc2 animale2]
  (-> celle
      (imposta-animale rc1 animale1)
      (imposta-animale rc2 animale2)))



(defmulti avanza-animale
  (fn [celle [r c] animale] (:tipo animale)))

(defmethod avanza-animale nil
  [celle [r c] animale]
  celle)

; ===============================================================
; PESCI



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


; ==============================================================
; SQUALI
; - decrementa energia
; - ci sono pesci nelle celle adiacenti? se si mangia
;  - se mangia, ha abbastanza energia per il piccolino?
; - muove a caso se possibile muovere
; - se energia < 0 muore

(defn pesce-adiacente? [rc celle]
  (let [celle-da-guardare (map #(nuovopunto rc %) DIREZIONI)
        celle-con-pesci (filter #(cella-con-pesce? celle %) celle-da-guardare)]

    (cond
      (pos? (count celle-con-pesci))  (rand-nth celle-con-pesci)
      :else                           nil)))


(defmethod avanza-animale :SQUALO
  [celle [r c] animale]
  (let [squalo (decrementa-energia animale)
        rc-pesce (pesce-adiacente? [r c] celle)]

    (if rc-pesce
      ; mangio un pesce
      (let [squalo-grasso (incrementa-energia animale)]
        (muovi-animale celle
                       rc-pesce squalo-grasso
                       [r c]    nil))

      ; nulla da mangiare
      (let [nuovacella (nuovopunto [r c] (mossa-casuale))]
        (if (cella-vuota? celle nuovacella)
          (muovi-animale celle
                         nuovacella squalo
                         [r c]      nil)
          celle)))))



(defn avanza-gioco-cella [celle rc]
  (doall
     ;(println "Celle " rc " - " celle)
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


(defn join-val [dict chiave valore]
  (let [v0 (conj (get-in dict [chiave]) valore)]
    (assoc-in dict [chiave] v0)))

(defn conta-celle
  "Restituisce un hash con tutte le celle per tipo"
  [celle]
  (reduce #(update-in %1 [%2] inc) {:PESCE 0 :SQUALO 0}
          (for [rc (coordinate-gioco)]
            (:tipo (get-in celle rc)))))

(defn run-turno-gioco
  "Questo metodo è chiamato dal bottone"
  []
  (let [t0 (.getTime (js/Date.))
        new-cells (avanza-gioco (:pf @app-state))
        tipi-celle (conta-celle new-cells)
        ;x (println "TC:" tipi-celle)
        t1 (.getTime (js/Date.))
        dur (- t1 t0)]
    (do
      (imposta-campo-di-gioco new-cells)
      (swap! app-state update-in [:turn] inc)
      (swap! app-state join-val :n_pesci (:PESCE tipi-celle))
      (swap! app-state join-val :n_squali (:SQUALO tipi-celle))
      (swap! app-state join-val :loop_ms dur))))



;
; Plots the playfield, it is a table of size w*h
;

(defmulti plot-cell (fn [data] (:tipo data)))

(defmethod plot-cell :PESCE
  [data]
  [:td {:class (name (:colore data))}  (str "P" (:energia data)) ] )

(defmethod plot-cell :SQUALO
  [data]
  [:td {:class (name (:colore data))}  (str "<" (:energia data)) ] )

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
   {:cell-spacing "0"}

   [:thead>tr
    [:td {:width "20px"} "-"]
    (map #(vec [:th {:width "20px"} (str %)]) (range PLAYFIELD-WIDTH))]

   [:tbody

    (let [table (:pf @app-state)]
      (for [row (range PLAYFIELD-HEIGHT)]
        (plot-row row (get table row)))) ]]

  ;; BOTTONI

  (str "Pesci: " (first (:n_pesci @app-state))
          " - Squali: " (first (:n_squali @app-state))
          " - Loop: " (first (:loop_ms @app-state)) "ms")

  [:br]

  [:input {:type "button"
             :value (str "Avanza #" (:turn @app-state))
             :on-click run-turno-gioco}]

  [:input {:type "button" :value "Reset!"
     :on-click
     #(doall
       (imposta-campo-di-gioco (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT))
       (swap! app-state assoc-in [:turn] 0))}]

    [:input {:type "button" :value "Pesce 1,1"
     :on-click
     #(imposta-campo-di-gioco
       (imposta-animale (:pf @app-state) [1 1] (crea-nuovo-pesce)))}]

     [:input {:type "button" :value "Squalo 8,8"
      :on-click
      #(imposta-campo-di-gioco
        (imposta-animale (:pf @app-state) [8 8] (crea-nuovo-squalo)))}]


  ]





  )

     ;[:pre (str @app-state)]




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











