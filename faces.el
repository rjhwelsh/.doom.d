;;; faces.el -*- lexical-binding: t; -*-

;; User-defined faces
(after! org
  (face-spec-set 'org-block
                 '((
                    ((class color)(min-colors 8))
                    :background "gray5"
                    :foreground "DarkGoldenrod3"
                    ))
                 'face-override-spec
                 )

  (defface org-keyword
    '((
       t
       :weight extra-bold
       :box (
             :style released-button )
       :inherit (org-todo org-tag org-level-3)
       ))
    "Org base face for todo keywords and tags"
    :group 'org-faces
    )
  (defface org-black
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "black"
       :foreground "white"
       ))
    "Org color"
    :group 'org-faces
    )
  (defface org-red
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "dark red"
       :foreground "cornsilk"
       ))
    "Org color"
    :group 'org-faces
    )
  (defface org-orange
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "tan4"
       :foreground "wheat"
       ))
    "Org color"
    :group 'org-faces
    )
  (defface org-yellow
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "dark olive green"
       :foreground "yellow"
       ))
    "Org color"
    :group 'org-faces
    )


  (defface org-green
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "dark green"
       :foreground "khaki"
       ))
    "Org color"
    :group 'org-faces
    )


  (defface org-cyan
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "dark cyan"
       :foreground "green yellow"
       ))
    "Org color"
    :group 'org-faces
    )


  (defface org-blue
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "navy"
       :foreground "turquoise"
       ))
    "Org color"
    :group 'org-faces
    )


  (defface org-magenta
    '((
       default
       :inherit (org-keyword))
      (
       ((class color) (min-colors 8))
       :background "dark magenta"
       :foreground "cyan"
       ))
    "Org color"
    :group 'org-faces
    )
  )
