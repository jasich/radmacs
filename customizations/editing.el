;;;;
;; Multiple cursors
;;;;
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key [(super d)] 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Increase line height
(setq-default line-spacing 5)

;; Show cursor position
(column-number-mode 1)

;;;;
;; Company for auto complete
;;;;

(global-company-mode)

;; Cleanup whitepspace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; I love being able to "Open above" the current line
(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line-below ()
  "Insert an empty line below the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (indent-according-to-mode))

(global-set-key [(control return)] 'smart-open-line-above)
(global-set-key [(control shift return)] 'smart-open-line-below)


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

;; Move a line (or lines) up or down in the buffer
(global-set-key [(control super down)]  'move-line-region-down)
(global-set-key [(control super up)]  'move-line-region-up)


;; Navigate up and down
(global-set-key [(super down)] 'end-of-buffer)
(global-set-key [(super up)]   'beginning-of-buffer)

(setq mac-command-modifier 'super)

;; Duplicate current line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  ;; (next-line 1)
  (forward-line 1)
  (yank))
(global-set-key [(super shift d)] 'duplicate-line)

;; Get electric with it.
(setq electric-indent-mode nil)
(electric-indent-mode 1)

(defun comment-dwim-line (&optional arg)
  "ARG Replacement for the `comment-dwim` command.  If no region is selected and current
line is not blank and we are not at the end of the line, then comment current line.
Replaces default behaviour of `comment-dwim`, when it inserts comment at the end of the
line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; Comment/Uncomment with cmd-s
(global-set-key (kbd "s-/") 'comment-dwim-line)

;; Don't use tabs, use 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(setq-default fill-column 120)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable Flycheck
(global-flycheck-mode)

;; ;; Require Flymake
;; (require 'flymake-easy)

;; Code folding
(global-set-key (kbd "C-c C-f") 'hs-hide-all)
(global-set-key (kbd "C-c C-F") 'hs-toggle-hiding)
(global-set-key (kbd "C-c M-f") 'hs-show-all)

(require 'editorconfig)
(editorconfig-mode 1)
