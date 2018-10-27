(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; highlight matching parens, brackets, braces, etc...
(show-paren-mode 1)

;; configure those parens
(electric-pair-mode 1)

;; ;; Parinfer
;; (global-set-key (kbd "C-,") 'parinfer-toggle-mode)
;; (setq parinfer-extensions
;;       '(defaults       ; should be included.
;;          pretty-parens  ; different paren styles for different modes.
;;          smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;          smart-yank))   ; Yank behavior depend on mode.

;; (add-hook 'clojure-mode-hook #'parinfer-mode)
;; (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;; (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;; (add-hook 'scheme-mode-hook #'parinfer-mode)
;; (add-hook 'lisp-mode-hook #'parinfer-mode)
