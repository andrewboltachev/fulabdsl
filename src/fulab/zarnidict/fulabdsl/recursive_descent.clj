(ns fulab.zarnidict.fulabdsl.recursive-descent)
(use 'regexpforobj.core)

(defn recursive-descent-inner [x r f]
  ;(println "h is" h)
  ;(println "t is" t)
  (let [f (fn [beg end]
            ;(println beg end)
            (or
              (and (= beg :open) (= end :clse))
              (f beg end)
              )
            )
        recursive-descent-inner (fn [x r] (recursive-descent-inner x r f))
        [h & t] x
        r (or r [])
        k (if (or (= h :open) (= h :clse)) h (:value h))
        payload (if-not (or (= h :open) (= h :clse)) (:payload h))
        ]
    (if-not (nil? h)
      (cond
        (= k :open)
        (let [t1r1 (recursive-descent-inner t [])]
          (if (is_parsing_error? t1r1)
            t1r1
            (let [e (f payload (first (last t1r1)))]
              (if (and (is_parsing_error? e) (not (nil? e)))
                e
                (recursive-descent-inner
                  (first t1r1)
                  (conj r {:tag
                           payload
                           :value (vec
                                    (rest (last t1r1))
                                    )
                           })
                  )
                )
              )
            )
          )
        ;
        (= k :clse)
        [t (cons payload r)]
        ;
        :else
        (recursive-descent-inner
          t
          (conj r payload)
          )
        ;
        )
        [t r]
      )
    )
  )

;(defn open-clse-stats [tags]
;  (mapcat (fn [k] (filter #(= (:value %) k) x)) [:open :clse])
;  )

(defn recursive-descent-maker [f]
  (fn [x]
    (if (apply not=
          (map (fn [k] (count (filter #(= (:value %) k) x))) [:open :clse])
         )
      {:error :open-clse-count-mismatch
       :context (identity
        (map (fn [k] [k (frequencies (map :payload (filter #(= (:value %) k) x)))]) [:open :clse])
                  )
       }
      (let [
            r (recursive-descent-inner (cons :open (conj (vec x) :clse)) [] f)
            ]
        (if 
          (is_parsing_error? r)
          r
          (:value (first (last r)))
          )
        )
      )
    )
  )

#_(def recursive-descent
  (recursive-descent-maker
    #(if-not
       (or (= %1 %2) (and (= %1 "m1") (= %2 "m")))
       {:error :tags-mismatch :context [%1 %2]}
       )
    )
  )
