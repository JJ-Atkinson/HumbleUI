(in-ns 'io.github.humbleui.ui)

(util/deftype+ Translate []
  :extends AWrapperNode 
  protocols/IComponent  
  (-draw-impl [_ ctx bounds viewport ^Canvas canvas]
    (let [[_ opts _] (parse-element element)
          dx         (dimension (or (:dx opts) 0) bounds ctx)
          dy         (dimension (or (:dy opts) 0) bounds ctx)
          child-bounds (util/irect-xywh
                       (+ (:x bounds) dx)
                       (+ (:y bounds) dy)
                       (:width bounds)
                       (:height bounds))]
      (draw child ctx child-bounds viewport canvas))))

(defn- translate-ctor [opts child]
  (map->Translate {}))
