;; -*- no-byte-compile: t; -*-
;;; app/matrix/packages.el

(package! matrix-client
  :pin "d2ac55293c96d4c95971ed8e2a3f6f354565c5ed"
  :recipe
  (:host github
   :repo "alphapapa/matrix-client.el")
  )
