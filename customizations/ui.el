;; Get all of the icons
(require 'all-the-icons)

;;;;
;; Themes
;;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; the current theme in use
;; (load-theme 'zenburn t)
(load-theme 'atom-one-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("68f7a53f5f1a8d30e5cd2d119fe6ecddb081bfe61bc427ca20eefd0abfada488" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;
;; Fonts
;;;;

(set-face-attribute 'default nil :height 190)

;;;;
;; Neotree
;;;;

(require 'neotree)

(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons))
(setq neo-autorefresh nil)
(setq neo-smartopen t)

;;;;
;; Line numbers
;;;;
(global-linum-mode 1)

(add-hook 'term-mode-hook 'my-inhibit-global-linum-mode)
(defun my-inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

;; Powerline
(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DodgerBlue"
                    :box nil)
