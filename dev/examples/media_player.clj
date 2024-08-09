(ns examples.media-player
  (:require [clojure.math :as math]
            [examples.shared :as shared]
            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.util :as util]
            [io.github.humbleui.signal :as signal]
            [io.github.humbleui.font :as font]
            [io.github.humbleui.paint :as paint]
            [io.github.humbleui.protocols :as protocols]
            [io.github.humbleui.typeface :as typeface]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.window :as window]
            [examples.effects :as effects])
  (:import [io.github.humbleui.skija FilterTileMode ImageFilter]))

(defn blur [radius] (ImageFilter/makeBlur radius radius FilterTileMode/CLAMP))

(def max-height-top-bar 140)

(def min-height-top-bar 60)

(def bottom-height 70)

(defn lerp
  [in-min in-max out-min out-max X]
  (let [scale (/ (- out-max out-min) (- in-max in-min))]
    (-> (- X in-min)
        (* scale)
        (+ out-min))))

(def song-titles
  [{:title "Lies Lies Lies",
    :artist "Morgan Wallen",
    :album "Lies Lies Lies",
    :length "3:18"}
   {:title "Sunshine Shine",
    :artist "Blanco Brown",
    :album "Sunshine Shine",
    :length "3:02"}
   {:title "Deadwood", :artist "Joe Jordan", :album "Deadwood", :length "2:36"}
   {:title "THE READNECK SONG",
    :artist "HARDY",
    :album "the mocking bird and the crow",
    :length "3:56"}
   {:title "Remember Him That Way",
    :artist "Luke Combs",
    :album "Fathers & Sons",
    :length "3:56"}
   {:title "High Time",
    :artist "Nickleback",
    :album "Get Rollin'",
    :length "3:55"}
   {:title "Sweet Home Alabama",
    :artist "Lynyrd Skynrd",
    :album "Second Helping",
    :length "4:44"}
   {:title "Hooch", :artist "Drew Green", :album "Hoooch", :length "2:25"}])

(def yellow-stroke #(paint/stroke 0x80FFDB2C (ui/scaled 2)))

(def green-fill #(paint/fill 0xFF00FF00))

(ui/defcomp simple-link
            [action text]
            [ui/clickable {:on-click (fn [_] (action))}
             (fn [state] [ui/column {:gap 2} [ui/label {} text]
                          [ui/rect
                           {:paint (if (:hovered state)
                                     (paint/fill 0xFFFFFFFF)
                                     (paint/fill 0x00000000))}
                           [ui/size {:height 2}]]])])

(ui/defcomp
  big-play-pause
  [size]
  [ui/clickable {:on-click (fn [_])}
   (fn [state] [ui/size {:width size, :height size}
                [ui/align {:x :center, :y :center}
                 [ui/rect {:paint (green-fill), :radius 50}
                  (let [inner-size (if (:hovered state) (+ size 3) size)]
                    [ui/size {:width inner-size, :height inner-size}])]]])])

(ui/defcomp
  top-element
  [*scroll-offset]
  (let [font-size (- 26 (min 10 (/ (or @*scroll-offset 0) 5)))
        total-size (max min-height-top-bar
                        (- max-height-top-bar (or @*scroll-offset 0)))
        blur-size (lerp max-height-top-bar
                        min-height-top-bar
                        (/ max-height-top-bar 2)
                        min-height-top-bar
                        total-size)
        button-size
          (lerp max-height-top-bar min-height-top-bar 50 40 total-size)]
    [ui/align {:y :top}
     [ui/stack
      [ui/align {:y :top}
       [ui/backdrop {:filter (blur 25)}
        [ui/rect {:paint (paint/fill 0x10FFFFFF)}
         [ui/size {:height blur-size}]]]]
      [ui/align {:x :left, :y :center, :child-y :center}
       [ui/size {:height total-size}
        [ui/padding {:horizontal 25}
         [ui/row {:gap 25} [big-play-pause button-size]
          [ui/align {:y :center}
           [ui/with-context
            {:fill-text (paint/fill 0xFFFFFFFF), :font-cap-height font-size}
            [ui/label "Discover Weekly"]]]]]]]]]))

(ui/defcomp bottom-element
            []
            [ui/align {:y :bottom}
             [ui/backdrop {:filter (blur 25)}
              [ui/rect {:paint (paint/fill 0x10FFFFFF)}
               [ui/size {:height bottom-height}
                [ui/padding {:horizontal 10, :vertical 5}
                 [ui/column {:gap 5}
                  [ui/with-context
                   {:hui.slider/stroke-thumb (green-fill),
                    :hui.slider/fill-track-active (green-fill)}
                   [ui/slider {:max 100}]]
                  [ui/padding {:horizontal 4}
                   [ui/stack
                    [ui/align {:x :left, :y :center}
                     [ui/column {:gap 5}
                      [ui/with-context
                       {:fill-text (paint/fill 0xFFFFFFFF), :font-cap-height 12}
                       [ui/label "Hooch"]]
                      [ui/with-context
                       {:fill-text (paint/fill 0xFFBBBBBB),
                        :paint (paint/fill 0xFFBBBBBB),
                        :font-cap-height 8}
                       [simple-link #() "Drew Green"]]]]
                    [ui/align {:x :center} [big-play-pause 30]]
                    [ui/align {:x :right} [ui/label "hi"]]]]]]]]]])

(ui/defcomp
  song-element
  [{:keys [title artist album length]}]
  [ui/clip {:radius 6}
   [ui/with-context {:fill-text (paint/fill 0xFFFFFFFF)}
    [effects/card {:bg (paint/fill 0xFF101010)}
     [ui/padding {:horizontal 10, :vertical 5}
      [ui/row {:gap 10}
       ^{:stretch 1}
       [ui/clip
        [ui/padding {:top 2}
         [ui/align {:x :left, :y :center}
          [ui/column {:gap 4} [ui/label {:font-cap-height 12} title]
           [ui/with-context {:font-cap-height 8} (simple-link #() artist)]]]]]
       ^{:stretch 1.8}
       [ui/clip
        [ui/align {:x :left, :y :center}
         [ui/with-context {:font-cap-height 10} (simple-link #() album)]]]
       ^{:stretch 0.5}
       [ui/clip
        [ui/align {:x :right, :y :center}
         [ui/label {:font-cap-height 10} length]]]]]]]])

(ui/defcomp song-elements
            []
            [ui/column {:gap 0}
             (for [song (apply concat (repeat 5 song-titles))]
               (song-element song))])

(ui/defcomp list-body
            [*scroll-offset]
            [ui/padding {:padding 13}
             [ui/align {:y :top}
              [ui/column {:gap 0}
               [ui/size {:height (+ 40 (/ max-height-top-bar 2))}]
               [song-elements] [ui/size {:height (+ 20 bottom-height)}]]]])

(defn ui
  []
  (let [*scroll-offset (signal/signal 0)]
    [ui/stack
     [ui/rect {:paint (paint/fill 0xFF101010)}
      [ui/vscroll {:offset *scroll-offset} [list-body *scroll-offset]]]
     [top-element *scroll-offset] [bottom-element]]))
