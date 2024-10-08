(in-ns 'io.github.humbleui.ui)

(util/deftype+ Label [size ^TextLine line ^Font font features-ctx]
  :extends ATerminalNode
  protocols/IComponent
  (-measure-impl [this ctx cs]
    size)
  
  (-draw-impl [this ctx bounds viewport ^Canvas canvas]
    (let [[_ opts _] (parse-element element)
          paint      (or (:paint opts) (:fill-text ctx))]
      (.drawTextLine canvas line (:x bounds) (+ (:y bounds) (:height size)) paint)))
  
  (-should-reconcile? [_this ctx new-element]
    (and
      (= element new-element)
      (let [[_ new-opts _] (parse-element new-element)]
        (and
          (identical? font (get-font new-opts))
          (= features-ctx (:font-features ctx))))))
  
  (-unmount-impl [this]
    (.close line)))

(defn- label-impl [opts & texts]
  (let [font         (get-font opts)
        features-ctx (:font-features *ctx*)
        features     (cond-> ShapingOptions/DEFAULT
                       (not (empty? features-ctx))
                       (.withFeatures (str/join " " features-ctx))
                       (not (empty? (:font-features opts)))
                       (.withFeatures (str/join " " (:font-features opts))))
        text         (str/join texts)
        line         (.shapeLine shaper text font features)
        metrics      (.getMetrics font)
        size         (util/ipoint
                       (math/ceil (.getWidth line))
                       (math/ceil (.getCapHeight metrics)))]
    (map->Label 
      {:size         size
       :line         line
       :font         font
       :features-ctx features-ctx})))

(defn- label-ctor [& texts]
  (let [[_ opts texts] (parse-element (util/consv nil texts))]
    (util/vector* label-impl opts
      (map signal/maybe-read texts))))
