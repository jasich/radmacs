;;;;
;; Files
;;;;

;; The folowing function allows you to rename the file you are working on

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; Projectile global mode
(projectile-global-mode +1)
(setq projectile-enable-caching t)

;; Kind of like Project Manager in Atom
(global-set-key (kbd "s-p") 'projectile-switch-project)

;; Kind of like Fuzzy Search in Atom
(global-set-key (kbd "s-t") 'projectile-find-file)

;; Fuzzy Search using IDO
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

;; Find current file when opening neotree
(setq neo-smart-open t)

;; Don't force a fixed size on neotree
(setq neo-window-fixed-size nil)
;; Set initial width
(setq neo-window-width 40)

;; Use projectile & neotree
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; Toggle neotree like Atom, could use 'neotree-toggle instead
(global-set-key (kbd "s-\\") 'neotree-project-dir)

(menu-bar-mode -1)          ;hide menu-bar
(scroll-bar-mode -1)        ;hide scroll-bar
(tool-bar-mode -1)          ;hide tool-bar

;; Save backups to a different directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Put auto-save files in different directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Global Search
(global-set-key [(super shift f)] 'helm-projectile-ag)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
