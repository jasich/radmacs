;;;;
;; Packages
;;;;
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Define all the packages I use for emacs
(defvar my-packages
  '(parinfer
    clojure-mode
    clojure-mode-extra-font-locking
    clj-refactor
    cider
    dash
    company
    multiple-cursors
    atom-one-dark-theme
    neotree
    ace-window
    flycheck
    markdown-mode
    all-the-icons
    ;; yasnippet
    ;; clojure-snippets
    web-mode
    ;; nyan-mode
    projectile
    powerline
    flx-ido
    haml-mode
    yaml-mode
    coffee-mode
    rspec-mode
    rubocop
    exec-path-from-shell
    rainbow-delimiters
    ruby-block
    magit))

;; Get all the packages!
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Adds /usr/local/bin to exec path so terminal can find my executables
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Adds an emacs vendor directory for manually placing .el files
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Load current environment from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;
;; Customization
;;;;

;; Add the customization directory to the load path so subsequent `load`
;; calls know where to look
(add-to-list 'load-path "~/.emacs.d/customizations")

(set-frame-font "Menlo 20")

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; no bell
(setq ring-bell-function 'ignore)

(load "buffers.el")
(load "ui.el")
(load "editing.el")
(load "clojure.el")
(load "ruby.el")
(load "parens.el")
(load "files.el")
(load "windows.el")
;; (load "fonts.el")
;; (load "snippets.el")
(load "web.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
   "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("68f7a53f5f1a8d30e5cd2d119fe6ecddb081bfe61bc427ca20eefd0abfada488" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (kibit-helper rspec-mode ruby-block exec-path-from-shell rubocop markdown-mode markdown-mode+ markdown-preview-eww coffee-mode yaml-mode web-mode dash paredit neotree multiple-cursors company clojure-mode-extra-font-locking cider)))
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers
      (quote
       (ruby-reek)))
     (flycheck-disabled-checkers . ruby-reek)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
