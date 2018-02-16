;; Enable rspec mode
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(add-hook 'ruby-mode-hook #'rubocop-mode)

;; idk if I have to run this all the time or not
(require 'ruby-block)
(ruby-block-mode t)

;;; Alignment rules for Ruby
;;; Originals from http://d.hatena.ne.jp/rubikitch/20080227/1204051280

(require 'align)

(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (group 2 3)
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal2
               (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

(add-to-list 'hs-special-modes-alist
       '(ruby-mode
         "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
         (lambda (arg) (ruby-end-of-block)) nil))
