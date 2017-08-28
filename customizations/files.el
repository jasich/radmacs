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

;; Toggle neotree like Atom
(global-set-key (kbd "s-\\") 'neotree-toggle)


;; Projectile global mode
(projectile-global-mode +1)
(setq projectile-enable-caching t)

;; Kind of like Project Manager in Atom
(global-set-key (kbd "s-p") 'projectile-switch-project)

;; Kind of like Fuzzy Search in Atom
(global-set-key (kbd "s-t") 'projectile-find-file)

;; Fuzzy Search using IDO
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

