(ns ewen.vseq.core
  (:require [goog.dom :as dom]))

(defn mount [root-node key-fn template renderer]
  (let [children (dom/getChildren root-node)
        key-map (loop [i (dec (aget children "length"))
                       key-map (transient {})]
                  (if (> i -1)
                    (let [child (aget children i)
                          k (.getAttribute child "data-vseq-id")]
                      (assert (not (nil? k)) "key must not be null")
                      (assert (not (contains? key-map k))
                              "Dusplicate key")
                      (recur (dec i) (assoc! key-map k child)))
                    key-map))]
    (aset root-node ::vseq {:key-map (persistent! key-map)
                            :key-fn (comp str key-fn)
                            :template template
                            :renderer renderer})))

(defn basic-renderer [root patch-ops]
  (doseq [[op & args] patch-ops]
    (case op
      :insert-node-first (dom/insertChildAt root (first args) 0)
      :insert-node-after (when (first args)
                           (dom/insertSiblingAfter (second args)
                                                   (first args)))
      :move-node-first (dom/insertChildAt root (first args) 0)
      :move-node-after (when (first args)
                         (dom/insertSiblingAfter (second args)
                                                 (first args)))
      :delete-node  (dom/removeNode (first args)))))

(defn dissoc-deleted-keys [key-map o-deleted]
  (apply (partial dissoc! key-map) o-deleted))

(defn add-deleted-patch-ops [patch-ops key-map o-deleted]
  (->> (keep (fn [k]
               (when-let [node (get key-map k)]
                 [:delete-node node]))
             o-deleted)
       (reduce conj! patch-ops)))

(defn add-o-moved-patch-op [patch-ops last-o-item o-item]
  (if last-o-item
    (conj! patch-ops [:move-node-after last-o-item o-item])
    (conj! patch-ops [:move-node-first o-item])))

(defn deleted-to-moved-patch-op [patch-ops last-o-item o-item]
  (conj! patch-ops [:move-node-after last-o-item o-item]))

(defn add-n-insert-patch-op [patch-ops last-o-item n-item]
  (if last-o-item
    (conj! patch-ops [:insert-node-after last-o-item n-item])
    (conj! patch-ops [:insert-node-first n-item])))

(defn make-node [template item key]
  (doto (template item) (.setAttribute "data-vseq-id" (str key))))

(defn patch! [root o-seq n-seq]
  (let [{:keys [key-map key-fn template renderer] :as vseq}
        (aget root ::vseq)]
    (loop [key-map (transient key-map)
           o-seq o-seq
           n-seq n-seq
           patch-ops (transient [])
           o-moved (transient #{})
           o-deleted (transient #{})
           last-o-key nil]
      (let [o-item (first o-seq)
            n-item (first n-seq)]
        (if (and (nil? o-item) (nil? n-item))
          ;; All nodes have been visited.
          (let [o-deleted (persistent! o-deleted)
                renderer (partial renderer root)]
            ;; Add patch-ops for to be deleted nodes.
            (-> (add-deleted-patch-ops
                 patch-ops key-map o-deleted)
                persistent! renderer)
            ;; Remove keys of deleted nodes from the key map and attach the
            ;; new vseq to the root node.
            (->> (dissoc-deleted-keys key-map o-deleted)
                 persistent!
                 (assoc vseq :key-map)
                 (aset root ::vseq)))
          (let [o-key (key-fn o-item)
                n-key (key-fn n-item)]
            (if (= o-key n-key)
              (recur key-map (rest o-seq) (rest n-seq)
                     patch-ops o-moved o-deleted o-key)
              (cond (and (not (nil? o-item))
                         (not (nil? n-item)))
                    (cond (get o-deleted n-key)
                          ;; The o-item has been marked as deleted.
                          ;; However, it should not be deleted but it
                          ;; should be moved instead.
                          (recur key-map o-seq (rest n-seq)
                                 (deleted-to-moved-patch-op
                                  patch-ops (get key-map last-o-key)
                                  (get key-map n-key))
                                 (conj! o-moved n-key)
                                 (disj! o-deleted n-key) n-key)
                          (get key-map n-key)
                          ;; Move o-item backward
                          (recur key-map o-seq (rest n-seq)
                                 (add-o-moved-patch-op
                                  patch-ops (get key-map last-o-key)
                                  (get key-map n-key))
                                 (conj! o-moved n-key)
                                 o-deleted n-key)
                          (get o-moved o-key)
                          ;; The o-item has already been handled
                          ;; (moved backward)
                          (recur key-map (rest o-seq) n-seq patch-ops
                                 o-moved o-deleted last-o-key)
                          :else
                          ;; Items are not null, keys don't match and all
                          ;; special cases have been handled.
                          ;; Mark the old node has deleted + insert the
                          ;; new-node at the current position.
                          (let [n-node (make-node template n-item n-key)]
                            (recur (assoc! key-map n-key n-node)
                                   (rest o-seq) (rest n-seq)
                                   (add-n-insert-patch-op
                                    patch-ops (get key-map last-o-key)
                                    n-node)
                                   o-moved (conj! o-deleted o-key) n-key)))
                    (nil? n-item)
                    (if (get o-moved o-key)
                      ;; The o-item has already been handled
                      ;; (moved backward). The fact that n-item is nil is not
                      ;; relevant here.
                      (recur key-map (rest o-seq) n-seq patch-ops
                             o-moved o-deleted last-o-key)
                      ;; The new node is nil and all special cases have been
                      ;; handled. Mark the old node as deleted.
                      (recur key-map (rest o-seq) (rest n-seq) patch-ops
                             o-moved (conj! o-deleted o-key) last-o-key))
                    (nil? o-item)
                    (cond (get o-deleted n-key)
                          ;; The o-item has been marked as deleted.
                          ;; However, it should not be deleted but it
                          ;; should be moved instead.
                          ;; The fact that o-item is nil is not relevant here.
                          (recur key-map o-seq (rest n-seq)
                                 (deleted-to-moved-patch-op
                                  patch-ops (get key-map last-o-key)
                                  (get key-map n-key))
                                 (conj! o-moved n-key)
                                 (disj! o-deleted n-key) n-key)
                          (get key-map n-key)
                          ;; A o-item has been moved backward.
                          ;; The fact that o-item is nil is not relevant here.
                          (recur key-map o-seq (rest n-seq)
                                 (add-o-moved-patch-op
                                  patch-ops (get key-map last-o-key)
                                  (get key-map n-key))
                                 (conj! o-moved n-key)
                                 o-deleted n-key)
                          :else
                          ;; o-item is nil and all special cases have been
                          ;; handled. Insert the new node at the current
                          ;; position.
                          (let [n-node (make-node template n-item n-key)]
                            (recur (assoc! key-map n-key n-node)
                                   (rest o-seq) (rest n-seq)
                                   (add-n-insert-patch-op
                                    patch-ops (get key-map last-o-key) n-node)
                                   o-moved o-deleted n-key)))
                    :else (throw (js/Error. "Illegal state"))))))))))

(comment
  (require '[goog.dom :as dom])

  (def root (dom/createDom "div" #js {:id "root"}))
  (dom/appendChild js/document.body root)

  (defn template [[tag attrs]]
    (dom/createDom (name tag) (clj->js attrs)))

  (dom/appendChild root (template [:div {:data-vseq-id 1}]))
  (dom/appendChild root (template [:div {:data-vseq-id 3}]))

  (let [vseq (mount root
                    (fn [x] (:data-vseq-id (second x)))
                    template basic-renderer)]
    (-> (patch! vseq
                [[:div {:data-vseq-id 1}] [:div {:data-vseq-id 3}]]
                [[:div {:data-vseq-id 3}] [:div {:data-vseq-id 2}] [:div {:data-vseq-id 1}]])
        :key-map))

  (let [vseq (mount root
                    (fn [x] (:data-vseq-id (second x)))
                    template basic-renderer)]
    (-> (patch! vseq
                [[:div {:data-vseq-id 1}] [:div {:data-vseq-id 3}]]
                [[:div {:data-vseq-id 3}] [:div {:data-vseq-id 4}] [:div {:data-vseq-id 5}] [:div {:data-vseq-id 1}] [:div {:data-vseq-id 7}]])
        :key-map))

  )
