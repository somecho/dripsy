(defpackage #:dripsy
  (:use #:cl #:arrows)
  (:nicknames :drip)
  (:export ;; public app vars
           :make
           :width
           :height
           :frame-num

           ;; app callbacks
           :on-key-pressed
           :on-resized

           ;; public draw API
           :line
           :eq-tri
           :set-color
           :use-fill!
           :no-fill!))
