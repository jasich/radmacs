;;;;
;; Cider
;;;;
(require 'cider)

;; Configure cider with figwheel
;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl))")

(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Keyboard shortcuts

;; For when the repl has too much stuff in it :(
(global-set-key (kbd "C-c x") 'cider-repl-clear-buffer)

;; Configure cider eldoc
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;;;;
;; Clojure mode
;;;;

;; clojure indenting - prevents "crazy" indenting
;; (setq clojure-defun-style-default-indent t)
;; (setq clojure-indent-style :always-indent)

(require 'clojure-mode-extra-font-locking)

;; clj-refactor
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
