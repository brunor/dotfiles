;; saner buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; map super to key to meta
(setq x-super-keysym 'meta)
;; map fn key (laptop) to meta as well
(setq ns-function-modifier 'control)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

;; remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(setq-default show-trailing-whitespace t)

;; no more beeps
(setq visible-bell t)

;; save backups and autosaves in a better place
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; pasting over something kills it
(delete-selection-mode 1)

;; textmate behavior for enter key
;;(global-set-key (kbd "<s-return>") 'textmate-next-line)

;; magit-status
;;(global-set-key (kbd "C-x g") 'magit-status)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t))

;; we're too lazy to type in yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; initialize it you never know when it might come in handy
(random t)

;; this thing enables case region commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; display line numbers to the right of the window
(global-linum-mode t)
(setq linum-format "%2d ")
;; wrap lines if longer than buffer width
(message "Enabling global-visual-line-mode...")
(global-visual-line-mode 1)

(desktop-save-mode 1)
