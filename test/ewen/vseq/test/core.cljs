(ns ewen.vseq.test.core
  (:require [ewen.vseq.core :refer [mount patch! basic-renderer]]
            [goog.dom :as dom]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]))

(defn template [[tag attrs]]
  (dom/createDom (name tag) (clj->js attrs)))

(def ids-gen (let [single-gen (gen/set (gen/choose 0 10))]
               (gen/not-empty (gen/tuple single-gen single-gen))))
(comment
  (gen/sample ids-gen)
  )

(defn read-keys [root-node]
  (let [children (dom/getChildren root-node)
        key-map (loop [i (dec (aget children "length"))
                       key-map (transient [])]
                  (if (> i -1)
                    (let [child (aget children i)
                          k (.getAttribute child "data-vseq-id")]
                      (assert (not (contains? key-map k))
                              "Duplicate keys")
                      (recur (dec i) (conj! key-map k)))
                    key-map))]
    (into '() (persistent! key-map))))

(def property
  (prop/for-all [[o-ids n-ids] ids-gen]
                (let [root (or (.getElementById js/document "root")
                               (dom/createDom "div" #js {:id "root"}))
                      make-vnode (fn [id] [:div {:data-vseq-id id}])
                      o-vnodes (map make-vnode o-ids)
                      n-vnodes (map make-vnode n-ids)]
                  (dom/appendChild js/document.body root)
                  (dom/removeChildren root)
                  (doseq [vnode o-vnodes]
                    (dom/appendChild root (template vnode)))
                  (let [vseq (mount root
                                    (fn [x] (:data-vseq-id (second x)))
                                    template basic-renderer)
                        o-key-map (:key-map vseq)
                        vseq (patch! root o-vnodes n-vnodes)
                        n-key-map (:key-map vseq)
                        read-key-map (read-keys root)]
                    (and (= (-> o-key-map keys set)
                            (-> (map str o-ids) set))
                         (= (-> n-key-map keys set)
                            (-> (map str n-ids) set))
                         (= read-key-map
                            (map str n-ids)))))))

(comment
  (tc/quick-check 100 property)
  )
