(ns livewator.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "This text is printed from src/livewator/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload


(defn buildrow
  "Crea una vettore riga di n elemeni ripetuti
   Serve per inizializzare il campo da gioco."
  [n-items value]
  (vec (repeat n-items value)))


(defn empty-playfield
  "Crea un campo da gioco vuoto; matrice w*h di elementi nil"
  [w h]
  (buildrow h (buildrow w nil))
)

(def PLAYFIELD-WIDTH 7)
(def PLAYFIELD-HEIGHT 5)


(defonce app-state (atom {:pf (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT)}))

; ==========================================================
; Animals
;
; {:tipo :PESCE }
;
; {:tipo :SQUALO :energia 3}
;






;
; Plots the payfield, it is a table of size w*h
;

(defn plot-cell [data]
  [:td (str "x" data)]

  )

(defn plot-row
  "[:tr
     [:td 'Matthew']
     [:td '26']]"
  [nrow rowdata]

  [:tr
    [:td (str nrow)]
    (map plot-cell rowdata)
    ]
  )



(defn plot-playfield []
  [:table.table.table-striped.table-bordered
   {:cell-spacing "0" :width "70%" }

   [:thead>tr
     [:td "-"]
     (map #(vec [:th (str %)]) (range PLAYFIELD-WIDTH))
    ]

   [:tbody

    (let [table (:pf @app-state)]
      (for [row (range PLAYFIELD-HEIGHT)]
        (plot-row row (get table row))))

    ]]
  )


(defn hello-world []
  [:h3 (:text @app-state)])

(reagent/render-component [plot-playfield]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)













