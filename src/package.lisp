(defpackage #:dripsy
  (:use #:cl)
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
           :eq-tri
           :set-color))
