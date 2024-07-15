(in-ns 'io.github.humbleui.ui)

(defn- clickable-state [phase]
  (case phase
    :default
    #{}
      
    :hovered
    #{:hovered}
    
    :hovered-held
    #{:hovered :held :pressed}
    
    :held
    #{:held}
    
    :hovered-unpressed
    #{:hovered :held :pressed}
    
    :unpressed
    #{:held}))

(core/deftype+ Clickable [*state
                          ^:mut phase
                          ^:mut clicks
                          ^:mut last-click]
  :extends AWrapperNode
  (-draw-impl [this ctx bounds canvas]
    (draw-child (:child this) ctx bounds canvas)
    (when (#{:unpressed :hovered-unpressed} phase)
      (set! phase (case phase
                    :unpressed         :default
                    :hovered-unpressed :hovered))
      (signal/reset-changed! *state (clickable-state phase))
      (force-render this (:window ctx))))

  (-event-impl [this ctx event]
    (when (= :mouse-move (:event event))
      (set! clicks 0)
      (set! last-click 0))
    
    (let [{:keys [on-click on-click-capture]} (parse-opts element)
          {x :x y :y}   event
          over?         (and x y (core/rect-contains? bounds (core/ipoint x y)))
          out?          (and x y (not over?))
          btn-down?     (and (= :mouse-button (:event event)) (:pressed? event))
          btn-up?       (and (= :mouse-button (:event event)) (not (:pressed? event)))
          phase'  (case phase
                    :default
                    (cond
                      over?     :hovered
                      :else     phase)
                          
                    :hovered
                    (cond
                      out?      :default
                      btn-down? :hovered-held
                      :else     phase)
                          
                    :hovered-held
                    (cond
                      btn-up?   :hovered-unpressed
                      out?      :held
                      :else     phase)
                          
                    :held
                    (cond
                      over?     :hovered-held
                      btn-up?   :default
                      :else     phase)
                          
                    :hovered-unpressed
                    (cond
                      btn-down? :hovered-held
                      out?      :unpressed
                      :else     phase)
                          
                    :unpressed
                    (cond
                      over?     :hovered-unpressed
                      :else     phase))
          clicked?      (and 
                          (= :hovered-held phase)
                          btn-up?)
          now           (core/now)
          _             (when clicked?
                          (when (> (- now last-click) core/double-click-threshold-ms)
                            (set! clicks 0))
                          (set! clicks (inc clicks))
                          (set! last-click now))
          event'        (cond-> event
                          clicked? (assoc :clicks clicks))
          state         @*state
          state'        (clickable-state phase')]
      
      (set! phase phase')
      
      (when (not= state state')
        (force-render this (:window ctx))
        (reset! *state state'))
      
      (or
        (and
          clicked?
          on-click-capture
          (core/invoke on-click-capture event'))
        (event-child child ctx event')
        (and
          clicked?
          on-click
          (core/invoke on-click event')))))
  
  (-should-reconcile? [_this _ctx new-element]
    (opts-match? [:*state] element new-element))
  
  (-child-elements [this ctx new-element]
    (let [[_ _ [child-ctor-or-el]] (parse-element new-element)]
      (if (fn? child-ctor-or-el)
        [[child-ctor-or-el @*state]]
        [child-ctor-or-el]))))

(defn- clickable-ctor
  "Element that can be clicked. Supports nesting (innermost will be clicked).
   
   Options are:
   
   :on-click         :: (fn [event]), what to do on click
   :on-click-capture :: (fn [event]), what to do on click before children
                        have a chance to handle it
   :*state           :: signal, controls/represent state
   
   Event is map with keys:
   
   :clicks  :: long, number of consequitive clicks
   
   *state contains a set that might include:
   
   :hovered :: mouse hovers over object
   :pressed :: mouse is held over object
   :held    :: mouse started over object and is still held"
  [opts child]
  (map->Clickable
    {:phase       :default
     :*state      (or (:*state opts) (signal/signal #{}))
     :clicks      0
     :last-click  0}))
