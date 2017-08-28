;;;;
;; Multiple cursors
;;;;
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;;;;
;; Company for auto complete
;;;;

(global-company-mode)


;;;;
;; Paredit
;;;;
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; configure those parens

;; highlight matching parens, brackets, braces, etc...
(show-paren-mode 1)

;; Cleanup whitepspace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Get electric with it.
(electric-pair-mode 1)
(electric-indent-mode 1)


;; I love being able to "Open above" the current line
(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control return)] 'smart-open-line-above)


;; Move region code, allows keybindings to move a line up and down.
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))

(global-set-key [(control shift down)]  'move-line-region-down)
(global-set-key [(control shift up)]  'move-line-region-up)


;; Tab around buffers
(global-set-key [C-tab] 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)


;; Navigate up and down
(global-set-key [(super down)] 'end-of-buffer)
(global-set-key [(super up)] 'beginning-of-buffer)

(setq mac-command-modifier 'super)

;; Duplicate current line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "s-d") 'duplicate-line)
