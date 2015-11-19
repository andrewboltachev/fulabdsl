(ns fulabdsl.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   [regexpforobj.core :refer [
                              InputChar
                              Seq
                              Star
                              Char
                              run
                              grammar_pretty
                              is_parsing_error?
                              ]]
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

(declare lines-to-header-and-body body-to-articles)

(def test-data
  ; ...
(clojure.string/split-lines (clojure.string/replace "# foo
# bar
# buz
word1
  [trn]foo [p]shrt.[/p][/trn]
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
    (->
      test-data
      ;ifipp
      lines-to-header-and-body
      :body
      body-to-articles
      parse-body-lines-of-articles
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

(defn lines-to-header-and-body [lines]
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


(defn body-to-articles [body]
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
      (assoc result :step :body-to-articles)
      (mywalk result)
      )
    )
  )

(defn r0001 [s]
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


(defn parse-body-lines-of-articles [articles]
  (map
   #(update % 1
            (fn [lines] (map line-value lines))
            ) 
    articles
    )
  )
