(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq js-indent-level 2)

;; (add-hook 'haml-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(custom-set-variables '(coffee-tab-width 2))
