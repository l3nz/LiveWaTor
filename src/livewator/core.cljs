(ns livewator.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is testing run-tests]]
            [livewator.model :as m]
            ))

(enable-console-print!)

;;(println "This text is printed from src/livewator/core.cljs. Go ahead and edit it and see reloading in action.")



;; ============================================================
;;   ClojureScript - the view
;; ============================================================




(defonce app-state (atom
      {:pf (m/empty-playfield m/PLAYFIELD-WIDTH m/PLAYFIELD-HEIGHT)
       :turn 0
       :n_pesci '(0)
       :n_squali '(0)
       :loop_ms  '(0)
       }))



(defn imposta-campo-di-gioco [celle]
  (swap! app-state assoc-in [:pf] celle) )


(defn join-val [dict chiave valore]
  (let [v0 (conj (get-in dict [chiave]) valore)]
    (assoc-in dict [chiave] v0)))



(defn run-turno-gioco
  "Questo metodo è chiamato dal bottone"
  []
  (let [t0 (.getTime (js/Date.))
        new-cells (m/avanza-gioco (:pf @app-state))
        tipi-celle (m/conta-celle new-cells)
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

(defmulti plot-cell (fn [data _] (:tipo data)))

(defmethod plot-cell :PESCE
  [data idx]
  ^{:key idx}
  [:td {:class (name (:colore data))}  (str "P" (:energia data)) ] )

(defmethod plot-cell :SQUALO
  [data idx]
   ^{:key idx}
   [:td {:class (name (:colore data))}  (str "<" (:energia data)) ] )

(defmethod plot-cell nil
  [data idx]
  ^{:key idx}
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
    ^{:key "0"} [:td  (str nrow)]
    ;(map plot-cell rowdata)
    (map-indexed (fn [idx itm] (plot-cell itm (str idx))) rowdata)
    ]
  )



(defn plot-playfield []

  [:div

  [:table.table.table-striped.table-bordered
   {:cell-spacing "0"}

   [:thead>tr
    [:th {:width "20px"} "-"]
    (map #(vec  [:th {:key (str %) :width "20px"} (str %)]) (range m/PLAYFIELD-WIDTH))]

   [:tbody

    (let [table (:pf @app-state)]
      (for [row (range m/PLAYFIELD-HEIGHT)]
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
       (imposta-campo-di-gioco (m/empty-playfield m/PLAYFIELD-WIDTH m/PLAYFIELD-HEIGHT))
       (swap! app-state assoc-in [:turn] 0))}]

    [:input {:type "button" :value "Pesce 1,1"
     :on-click
     #(imposta-campo-di-gioco
       (m/imposta-animale (:pf @app-state) [1 1] (m/crea-nuovo-pesce)))}]

     [:input {:type "button" :value "Squalo 8,8"
      :on-click
      #(imposta-campo-di-gioco
        (m/imposta-animale (:pf @app-state) [8 8] (m/crea-nuovo-squalo)))}]


  ]





  )



(reagent/render-component [plot-playfield]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)











