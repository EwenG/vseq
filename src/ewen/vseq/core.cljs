(ns ewen.vseq.core)


(comment

  (defn mount [root-node template renderer!]
    {:key-map {}
     :template template
     :renderer! renderer!})

  (defn dissoc-deleted-keys [key-map o-deleted]
    (apply dissoc! key-map o-deleted))

  (defn add-deleted-patch-ops [patch-ops key-map o-deleted]
    (->> (keep (fn [k]
                 (when-let [k (get key-map k)]
                   [:delete-node k]))
               o-deleted)
         (reduce conj! patch-ops)))

  (defn add-o-moved-patch-op [patch-ops last-o-item o-item]
    (conj! patch-ops [:move-node-after last-o-item o-item]))

  (defn add-n-insert-patch-op [patch-ops last-o-item n-item]
    (if last-o-item
      (conj! patch-ops [:insert-node-after last-o-item n-item])
      (conj! patch-ops [:insert-node-first n-item])))

  (defn patch! [vseq o-seq n-seq k-fn tmpl]
    (loop [key-map (transient (:key-map vseq))
           o-seq o-seq
           n-seq n-seq
           patch-ops (transient #{})
           o-moved (transient #{})
           o-deleted (transient #{})
           last-o-key nil]
      (let [o-item (first n-seq)
            n-item (first n-seq)]
        (if (and (nil? o-item) (nil? n-item))
          (-> (add-deleted-patch-ops
               patch-ops key-map
               (persistent! o-deleted))
              persistent! ((:renderer! vseq)))
          {:key-map (-> (dissoc-deleted-keys key-map o-deleted)
                        persistent!)
           :template (:template vseq)
           :renderer! (:renderer! vseq)}
          (let [n-key (k-fn n-item)
                o-key (k-fn o-item)]
            (when (= n-key o-key)
              (recur key-map (rest o-seq) (rest n-seq)
                     patch-ops o-moved o-deleted o-key))
            (cond (and (not (nil? o-item))
                       (not (nil? n-item)))
                  (cond (get key-map n-key)
                        ;; The o-item has been moved backward
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
                        (get o-deleted n-key)
                        ;; The o-item has been marked as deleted.
                        ;; However, it should not be deleted but it
                        ;; should be moved instead.
                        (recur key-map o-seq (rest n-seq)
                               (add-o-moved-patch-op
                                patch-ops (get key-map last-o-key)
                                (get key-map n-key))
                               (conj! o-moved n-key)
                               (disj! o-deleted n-key) n-key)
                        :else
                        ;; Items are not null, keys don't match and all
                        ;; special cases have been handled.
                        ;; Mark the old node has deleted + insert the
                        ;; new-node at the current position.
                        (let [n-node ((:template vseq) n-item)]
                          (recur (assoc! key-map n-key n-node)
                                 (rest o-seq) (rest n-seq)
                                 (add-n-insert-patch-op
                                  patch-ops (get key-map last-o-key) n-node)
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
                  (cond (get key-map n-key)
                        ;; A o-item has been moved backward.
                        ;; The fact that o-item is nil is not relevant here.
                        (recur key-map o-seq (rest n-seq)
                               (add-o-moved-patch-op
                                patch-ops (get key-map last-o-key)
                                (get key-map n-key))
                               (conj! o-moved n-key)
                               o-deleted n-key)
                        (get o-deleted n-key)
                        ;; The o-item has been marked as deleted.
                        ;; However, it should not be deleted but it
                        ;; should be moved instead.
                        ;; The fact that o-item is nil is not relevant here.
                        (recur key-map o-seq (rest n-seq)
                               (add-o-moved-patch-op
                                patch-ops (get key-map last-o-key)
                                (get key-map n-key))
                               (conj! o-moved n-key)
                               (disj! o-deleted n-key) n-key)
                        :else
                        ;; o-item is nil and all special cases have been
                        ;; handled. Insert the new node at the current
                        ;; position.
                        (let [n-node ((:template vseq) n-item)]
                          (recur (assoc! key-map n-key n-node)
                                 (rest o-seq) (rest n-seq)
                                 (add-n-insert-patch-op
                                  patch-ops (get key-map last-o-key) n-node)
                                 o-moved o-deleted n-key)))
                  :else (throw (js/Error. "Illegal state"))))))))

  ;; 1 2   3
  ;; 2 nil 3

  ;; 1 2 3 4 5
  ;; 1 3 2 4 5

  ;; 1 2 3   4
  ;; 2 3 nil 1
)
