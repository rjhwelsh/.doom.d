;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;
;; Org packages
(package! org-edna)

;; Org-cv packages
(package! ox-json)
(package! ox-hugo)
(package! org-cv :recipe
  (:host gitlab
   :repo "Titan-C/org-cv"))
(package! ox-awesomecv :recipe
  (:host gitlab
   ;; :repo "zzamboni/org-cv"
   :repo "rjhwelsh/org-cv"
   :local-repo "awesome-cv" ;; (doom sync && doom purge)
   ;; :local-repo "/home/hrothgar/import/git/diego_zamboni_org-cv"
   :branch "awesomecv"))

;; Email
(unpin! org-msg)

;; Jabber (and related packages)
(package! jabber)
(package! srv)
(package! fsm)

;; Misc packages
(package! rainbow-delimiters)
(package! which-key)
(package! xah-fly-keys)
(package! crux)
(package! scad-mode)
(package! pinentry)

;; Emacs sql for windows
(if (eq system-type 'windows-nt)
    (package! emacsql-sqlite3))
