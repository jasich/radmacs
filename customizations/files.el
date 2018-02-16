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
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Find current file when opening neotree
(setq neo-smart-open t)

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

(global-set-key (kbd "s-w") 'kill-this-buffer)

(menu-bar-mode -1)          ;hide menu-bar
(scroll-bar-mode -1)        ;hide scroll-bar
(tool-bar-mode -1)          ;hide tool-bar

;; Switching buffers
(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;; Switch buffers
(global-set-key [(super right)] 'xah-next-user-buffer)
(global-set-key [(super left)] 'xah-previous-user-buffer)
