;; -*- no-byte-compile: t; -*-
;;; app/matrix/packages.el

;; (package! matrix-client
;;   :pin "d2ac55293c96d4c95971ed8e2a3f6f354565c5ed"
;;   :recipe
;;   (:host github
;;    :repo "alphapapa/matrix-client.el")
;;   )

(package! ement
  :pin "c951737dc855604aba389166bb0e7366afadc533"
  :recipe
  (:host github
   :repo "alphapapa/ement.el")
  )

(package! plz
  :pin "7e456638a651bab3a814e3ea81742dd917509cbb"
  :recipe
  (:host github
   :repo "alphapapa/plz.el"))
