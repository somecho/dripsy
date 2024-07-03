(defpackage #:dripsy
  (:use #:cl)
  (:nicknames :drip)
  (:export ;; public app vars
           :make
           :on-key-pressed
           :on-resized
           :width
           :height
           :frame-num

           ;; public draw API
           :eq-tri
           :set-color))
