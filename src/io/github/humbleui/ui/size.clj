(in-ns 'io.github.humbleui.ui)

(core/deftype+ Size []
  :extends AWrapperNode
  protocols/IComponent
  (-measure-impl [_ ctx cs]
    (let [[_ opts _] (parse-element element)
          width      (some-> opts :width (dimension cs ctx))
          height     (some-> opts :height (dimension cs ctx))]
      (cond
        (and width height)
        (core/ipoint width height)
        
        (and width child)
        (assoc (measure child ctx (assoc cs :width width)) :width width)
        
        (and height child)
        (assoc (measure child ctx (assoc cs :height height)) :height height)
        
        width
        (core/ipoint width 0)
        
        height
        (core/ipoint 0 height)
        
        :else
        (core/ipoint 0 0)))))

(defn size
  ([opts]
   (size opts nil))
  ([opts child]
   (map->Size {})))

(defn node-size []
  (let [scale (or (:scale *ctx*) 1)
        w     (or (:width (:rect *node*)) 0)
        h     (or (:height (:rect *node*)) 0)]
    (core/point (/ w scale) (/ h scale))))

(defn use-size []
  (let [*size (signal/signal (core/point 0 0))]
    {:before-draw
     (fn []
       (reset! *size (node-size)))
     :value *size}))
