(ns fulabdsl.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   [regexpforobj.core :refer [
                              InputChar
                              Or
                              Seq
                              Star
                              Char
                              run
                              grammar_pretty
                              is_parsing_error?
                              ]]
    [fulabdsl.recursive-descent :refer [
                                        recursive-descent-maker]]
   )
  #_(:require
    )
  )
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])

(defn ifipp [x]
  (do
    (fipp x)
    x)
  )

#_(require 
    '[regexpforobj.core] :reload
  )

(declare parse-fulabdsl-lines)

(def test-data
  ; ...
(clojure.string/split-lines (clojure.string/replace "# foo
# bar
# buz
word1
  [trn]foo [p]shrt.[/p][/trn]
  [lang1][/lang1] [lang2][/lang2]
  [lang1][/lang1] [lang2][/lang2]
  
word2
  text1
  " #"  " (str \tab)))
  )

(defn main
  []
  (println "Hello, Peter!" (InputChar :aa))
  ;(run-tests 'fulabdsl.core)
  (
   ;identity
   fipp
    (parse-fulabdsl-lines
      test-data
      ;:tag-compare-fn #(do %1 %2 {:error :error})
      :line-first-level-process-fn (comp list #(do {:tag "text" :value %}))
      :grammar (Or [(Star (Seq [(Char "trn") (Star (Seq [(Char "lang1") (Char "lang2")]))])) (Char "text")])
      )
    )
  )

(deftest test1
  (is (= (+ 2 3) 5)))


;; ----------- functions ----------

(defn- line-no [line]
  (if (coll? line)
    (first line))
  )

(defn- line-value [line]
  (if (coll? line)
    (second line)
    line
    )
  )

(defn lines-to-header-and-body1 [lines]
  (let [lines (map-indexed
                (fn
                  [number line]
                  ; (vary-meta line assoc :line-no number)
                  [(inc number) line]
                  )
                lines)
        condition #(= (first
                        (line-value %)
                        ) \#)]
    (into
      {}
      (for
        [[k v] {:header take-while :body drop-while}]
        [k (v condition lines)]
        )
      )
    )
  )


(defn lines-to-header-and-body [lines & options]
  (let [{header :header body :body} (lines-to-header-and-body1 lines)]
    (with-meta body {:header header}) 
    )
  )


(defn body-to-articles [body & options]
  ;(println "body-to-articles called")
  (let [
        token-type (fn [line] (cond
                      (empty? line)
                      :empty

                      (= line "\t")
                      :ending

                      (= (first line) \tab)
                      :body-line

                      :else
                      :word
                      )
                     )
        annotated-body (map
          #(let [value (line-value %)
                 current-token-type (token-type value)]
             (InputChar current-token-type [(line-no %)
                                            (if (= current-token-type :body-line)
                                              (clojure.string/replace
                                                value
                                                #"^\t"
                                                ""
                                                )
                                              value)
                                                                    ])
             )
          body
          )
        g (Star
            (Seq
              [
               (Char :word)
               (Star
                 (Char :body-line)
                 )
               (Char :ending)
               ]
              :article
              )
            :articles
            )
          ;_ (fipp g)
        result (run g annotated-body)
        ;_ (fipp result)
        ;_ (fipp (grammar_pretty result))
        mywalk (fn [s]
                 (clojure.walk/postwalk
                   (fn [x]
                     (if (map? x)
                       (cond
                         (= (:payload x) :articles)
                         (:value x)

                         (= (:payload x) :article)
                         (let [v (:value x)]
                           [
                            (-> x :value first :payload line-value)
                            (map :payload (-> x :value second :value))
                            ]
                           )

                         :else
                         x
                         )
                       x
                       )
                     )
                   s
                   )
                 )
        ]
    (if (is_parsing_error? result)
      result ;(assoc result :step :body-to-articles)
      (mywalk result)
      )
    )
  )


; --------------- tag parsing -------------

(defn- r0001 [s]
  ;(println "got s" s)
  (let [r #"[\n]|\[\w+\]|\[\/\w+\]"
        a (clojure.string/split s r)
        b (re-seq r s)
        m (apply max (map count [a b]))

        to-length #(loop [x %1] (if-not (= (count x) %2) (recur (conj x "")) x))
        a (to-length a m)
        b (to-length b m)
        ]
    (interleave
      a
      b
      )
    )
  )

(defn- is_tag? [x]
  (and
    (> (count x) 2)
    (= (first x) \[)
    (= (last x) \])
    )
  )

(defn- is_opening_tag? [x]
  (and
    (is_tag? x)
    (not= (second x) \/)
    )
  )

(defn- is_closing_tag? [x]
  (and
    (is_tag? x)
    (= (second x) \/)
    )
  )

(defn- clear-tag-brackets [tag]
  (->
    tag
    (clojure.string/replace "[/" "")
    (clojure.string/replace "[" "")
    (clojure.string/replace "]" "")
    )
  )

(defn r0002 [body]
  (mapv
    (fn [x]
      (cond
        (is_opening_tag? x)
        (InputChar :open (clear-tag-brackets x))

        (is_closing_tag? x)
        (InputChar :clse (clear-tag-brackets x))

        :else
        (InputChar :text x)
        )
      )
      body
    )
  )


(defn Y [f]
    (#(% %) #(f (fn [& args] (apply (% %) args)))))

(defn- process-article-line-tree [process-transducer]
  (Y (fn [f] (fn [s] (cond (sequential? s) (transduce process-transducer conj (map f s)) (map? s) (into {} (map (fn [x] (update x 1 (fn [v] (f v))))) s) :else s))))
  )


(defn- process-article-line-first-level [s process-fn]
  (mapcat (fn [x]
         (cond
           (string? x)
           (process-fn x)

           :else
           [x])
         ) s)
  )


(defn parse-body-lines-of-articles [articles & options]
  ;(println "parse-body-lines-of-articles called")
  (map
   (fn [article] (update article 1
            (fn [lines] (map #(do
                                [(line-no %)
                                (->
                                  %
                                  line-value
                                  r0001
                                  r0002
                                  ((recursive-descent-maker
                                     (:tag-compare-fn (apply hash-map options))
                                     )
                                   )
                                  
                                  #_((fn [article & a]
(println "article" article)
(newline)
(println "a" a)
(newline)
(newline)
                                            ""
                                            #_(update article 1 (fn [line]
                                                                            
                                                                    [(line-no line)
                                                                     (line-value line)]
                                                                    ))) articles)
                                  #_((clear-and-postprocess-article-lines
                                    identity))
                                  ((process-article-line-tree
                                    ;(filter (fn [x] (not (and (string? x) (partial re-matches #"^\s*$" x)))))
                                     (filter (partial not= ""))
                                    ))

                                  (process-article-line-first-level
                                    (fn [x] (if
                                              (re-matches #"^\s*$" x)
                                              []
                                              [x]
                                              ))
                                    )
                                  (process-article-line-first-level
                                     (:line-first-level-process-fn (apply hash-map options))
                                    )
                                   
                                  )
                                 ]
                                  ) lines))
            ))
    articles
    )
  )

(defn join-lines-tags [articles & options]
  (map
   (fn [article] (update article 1
                         (fn [lines] (mapcat line-value lines))
                         ))
    articles)
  )

(defn apply-grammar [articles & options]
  (let [grammar (:grammar (apply hash-map options))]
    (loop [[head & tail] articles
           result []]
      (let [[word & [article-body]] head]
        (if
          (nil? head)
          result
          (let [input (map #(InputChar (:tag %) (:value %)) article-body)
                parsed (run grammar input)]
            (if (is_parsing_error? parsed)
              (if-not (:error parsed)
                (assoc parsed :error :empty-grammar)
                (assoc parsed :article word)
                )
              (recur
                tail
                (conj result parsed)
                )
              )
            )
          )
        )
      )
    )
  )

; ---------- error checkers / steps -----------
(def steps-names
        (apply hash-map [
   lines-to-header-and-body :lines-to-header-and-body
   body-to-articles :body-to-articles
   parse-body-lines-of-articles :parse-body-lines-of-articles
   join-lines-tags :join-lines-tags
   apply-grammar :apply-grammar
   ])
  )

(def steps-data
  [
   [lines-to-header-and-body #(do % false)]
   [body-to-articles (fn [x] (when (is_parsing_error? x) x))]
   [parse-body-lines-of-articles
    (fn [s] 
      (let [articles (map second s)
            x (mapcat identity articles)
            x (filter (comp map? :value) (map #(do {:line-no (line-no %) :value (line-value %)}) x))
            x (filter (comp is_parsing_error? :value) x)
            x (map #(do (assoc (:value %) :line-no (:line-no %))) x)
            ]
        (when-not (empty? x)
             {:error :multiple :context x}
          )
        )
      )
    ] ; (fn [x] (when (is_parsing_error? x) x)) ; TODO
   [join-lines-tags
    ;(fn [& _] false)
    (fn [s] 
      (let [x (into {} (comp (map (fn [[k v]]
                                       [k (filter string? v)]
                                       ))
                             (filter (comp not empty? second))
                             ) s)
            ]
        (when-not (empty? x)
             {:error :multiple :context x}
          )
        )
      )
    ]
   [apply-grammar
    (fn [x] (when (is_parsing_error? x) x))]
   ]
  )

(def steps steps-data)

; ---------- composing function -----------

; TODO: line-preprocess-fn, tag-compare-fn ("inner" replacements?)
; join-lines (проверка что не осталось не-тегов)
; apply-grammar
; post-check
; 
; creating grammar...
(defn parse-fulabdsl-lines [lines & options]
  ;
  (let [options (apply hash-map options)
        ; assign default
        options (merge 
                  {:tag-compare-fn
                   (fn [beg end]
                                                            (if-not (= beg end)
                                                              {:error :tags-mismatch
                                                               :context [beg end]
                                                               }
                                                              )
                                                            )
                   :line-first-level-process-fn identity
                   :grammar nil
                   }
                  options)
        options (mapcat identity options)
        ]
    (loop
      [
      [head & tail] steps
        data lines
      ]
      (let [[func & [error-checker]] head
            step-name (steps-names func)]

        (if (nil? head)
          data
          (let [data (apply func data options)
                error-info (error-checker data)]
            (if error-info
              (assoc error-info :step step-name)
              (do
                ;(fipp data)
                ;(newline)
                (recur
                tail
                data
                )
                )
              )
            )
          )
        )
      )
    )
  )
