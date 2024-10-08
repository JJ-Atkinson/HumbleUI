(in-ns 'io.github.humbleui.ui)

(import '[io.github.humbleui.skija BreakIterator])

(defn- paragraph-layout [tokens max-width cap-height line-height]
  (util/loopr [positions (transient [])
               x         0
               y         0
               width     0
               height    0]
    [token tokens]
    (let [{:keys [^TextLine shaped blank?]} token
          token-width (.getWidth shaped)]
      (cond
        ;; first token ever
        (and (= x 0) (= y 0))
        (recur (conj! positions (util/point 0 cap-height)) token-width cap-height (max width token-width) cap-height)
        
        ;; blank — always advance, but don’t render
        blank?
        (recur (conj! positions nil) (+ x token-width) y width height)
        
        ;; next token fits on the same line
        (<= (+ x token-width) max-width)
        (recur (conj! positions (util/point x y)) (+ x token-width) y (max width (+ x token-width)) height)
        
        ;; have to start new line
        :else
        (recur (conj! positions (util/point 0 (+ y line-height))) token-width (+ y line-height) (max width token-width) (+ height line-height))))
    {:positions (persistent! positions)
     :width     width
     :height    height}))

(util/deftype+ Paragraph [tokens *layout line-height ^Font font metrics features-ctx]
  :extends ATerminalNode
  protocols/IComponent
  (-measure-impl [_ _ctx cs]
    (let [layout (util/cached *layout (:width cs)
                   #(paragraph-layout tokens (:width cs) (:cap-height metrics) line-height))]
      (util/ipoint
        (math/ceil (:width layout))
        (:height layout))))
  
  (-draw-impl [_ ctx bounds viewport ^Canvas canvas]
    (let [[_ opts _] (parse-element element)
          paint      (or (:paint opts) (:fill-text ctx))
          layout     (util/cached *layout (:width bounds)
                       #(paragraph-layout tokens (:width bounds) (:cap-height metrics) line-height))]
      (doseq [[pos token] (util/zip (:positions layout) tokens)
              :when pos]
        (.drawTextLine canvas (:shaped token) (+ (:x bounds) (:x pos)) (+ (:y bounds) (:y pos)) paint))))
  
  (-should-reconcile? [_ ctx new-element]
    (and
      (= element new-element)
      (let [[_ opts' _] (parse-element new-element)]
        (and
          (identical? font (font opts'))
          (= features-ctx (:font-features ctx))))))
  
  (-unmount-impl [this]
    (doseq [token tokens]
      (util/close (:shaped ^TextLine token)))))

(defn- paragraph-split-whitespace [s]
  (let [trimmed (str/trimr s)
        space   (subs s (count trimmed))]
    (if (pos? (count space))
      [trimmed space]
      [s])))

(defn- paragraph-words [text]
  (with-open [iter (BreakIterator/makeLineInstance)]
    (.setText iter text)
    (loop [words (transient [])
           start 0]
      (let [end (.next iter)]
        (if (= BreakIterator/DONE end)
          (persistent! words)
          (recur (reduce conj! words (paragraph-split-whitespace (subs text start end))) end))))))

(defn- paragraph-impl [opts & texts]
  (let [font         (get-font opts)
        line-height  (Math/ceil
                       (or (some-> opts :line-height (* (:scale *ctx*)))
                         (* 2 (:cap-height (font/metrics font)))))
        text         (str/join texts)
        features-ctx (:font-features *ctx*)
        features     (cond-> ShapingOptions/DEFAULT
                       (not (empty? features-ctx))
                       (.withFeatures (str/join " " features-ctx))
                       (not (empty? (:font-features opts)))
                       (.withFeatures (str/join " " (:font-features opts))))
        tokens       (mapv
                       (fn [token]
                         {:text   token
                          :shaped (.shapeLine shaper token font ^ShapingOptions features)
                          :blank? (str/blank? token)})
                       (paragraph-words text))]
    (map->Paragraph
      {:tokens       tokens
       :*layout      (atom nil)
       :line-height  line-height
       :font         font
       :metrics      (font/metrics font)
       :features-ctx features-ctx})))

(defn- paragraph-ctor [& texts]
  (let [[_ opts texts] (parse-element (util/consv nil texts))]
    (util/vector* paragraph-impl opts
      (map signal/maybe-read texts))))
