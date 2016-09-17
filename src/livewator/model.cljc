(ns livewator.model)

;; COSTANTI

(def IMG-PESCI {
    :PESCE1 {:pic "Pesce1.jpg"  :squalo? false}
    :PESCE2 {:pic "Pesce2.jpg"  :squalo? false}
    :PESCE3 {:pic "Pesce3.jpg"  :squalo? false}
    :PESCE4 {:pic "Pesce4.jpg"  :squalo? false}

    :SQUALO1 {:pic "Squalo1.jpg" :squalo? true}
    :SQUALO2 {:pic "Squalo2.jpg" :squalo? true}
    :SQUALO3 {:pic "Squalo3.jpg" :squalo? true}
    :SQUALO4 {:pic "Squalo4.jpg" :squalo? true}

})


(def TIPI-PESCI  (map first (filter #(= false (:squalo? (last %))) IMG-PESCI)))
(def TIPI-SQUALI (map first (filter #(= true  (:squalo? (last %))) IMG-PESCI)))

(def PLAYFIELD-WIDTH 30)
(def PLAYFIELD-HEIGHT 15)

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


(def SQUALO-RIPRODUCE 30)
(def SQUALO-NASCITA   15)
(def PESCE-RIPRODUCE   5)
(def PESCE-NASCITA     1)



;
; Costruisce il campo da gioco vuoto.
;


(defn buildrow
  "Crea una vettore riga di n elementi ripetuti
   Serve per inizializzare il campo da gioco."
  [n-items value]
  (vec (repeat n-items value)))

(defn empty-playfield
  "Crea un campo da gioco vuoto; matrice w*h di elementi nil"
  [w h]
  (buildrow h (buildrow w nil)))

(defn empty-default-playfield
    []
  (empty-playfield PLAYFIELD-WIDTH PLAYFIELD-HEIGHT))



; ==========================================================
; Animals
;
; {:tipo :PESCE }
;
; {:tipo :SQUALO :energia 3}
;

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
   :energia PESCE-NASCITA
   :turns   0})

(defn crea-nuovo-squalo []
  {:tipo    :SQUALO
   :colore  (rand-nth TIPI-SQUALI)
   :energia SQUALO-NASCITA
   :turns   0})

(defn setta-energia [animale value]
  (assoc-in animale [:energia] value))


(defn energia+ [animale delta-energia]
  (let [energia-corr (:energia animale)
        energia-nuova (+ energia-corr delta-energia)]
    (setta-energia animale energia-nuova)))



(defn incrementa-energia [animale]
  (update-in animale [:energia] inc))

(defn decrementa-energia [animale]
  (update-in animale [:energia] dec))


(defn leggi-cella [celle rc]
  (get-in celle rc))

(defn cella-vuota? [celle rc]
  (nil? (leggi-cella celle rc)))

(defn leggi-energia [celle rc]
  (:energia (leggi-cella celle rc)))

(defn leggi-tipo [celle rc]
  (:tipo (leggi-cella celle rc)))

(defn cella-con-pesce? [celle rc]
  (if (= :PESCE (leggi-tipo celle rc))
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
  (if (> PESCE-RIPRODUCE (:energia padre))
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
  [celle rc animale]
  (let [squalo (decrementa-energia animale)
        rc-pesce (pesce-adiacente? rc celle)]

    (if rc-pesce
      ; mangio un pesce
      (let [energia-pesce (leggi-energia celle rc-pesce)
            squalo-grasso (energia+ animale energia-pesce)]

        (if (> (:energia squalo-grasso) SQUALO-RIPRODUCE)
          ;; SI RIPRODUCE
          (let [squalo-padre (energia+ squalo-grasso (* -1 SQUALO-NASCITA))
                squalo-figlio (crea-nuovo-squalo)]
            (muovi-animale celle
                       rc-pesce squalo-padre
                       rc       squalo-figlio))

          ;; NON SI RIPRODUCE
          (muovi-animale celle
                       rc-pesce squalo-grasso
                       rc    nil)))

      ; nulla da mangiare
      (if (< 0 (:energia squalo))
        ;; provo a muovermi
        (let [nuovacella (nuovopunto rc (mossa-casuale))]
          (if (cella-vuota? celle nuovacella)
            ;; mi posso muovere
            (muovi-animale celle
                           nuovacella squalo
                           rc         nil)
            ;; non mi posso muovere
            celle))
        ; Squalo morto - bye bye
        (imposta-animale celle rc nil)
))))



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


(defn conta-celle
  "Restituisce un hash con tutte le celle per tipo"
  [celle]
  (reduce #(update-in %1 [%2] inc) {:PESCE 0 :SQUALO 0}
          (for [rc (coordinate-gioco)]
            (:tipo (get-in celle rc)))))







