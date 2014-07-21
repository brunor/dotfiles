


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(auto-complete
    projectile
    company
    rainbow-mode
    fill-column-indicator
    cursor-chg
    highlight-indentation
    highlight-symbol
    protobuf-mode
    ag
    go-mode
    go-autocomplete
    haskell-mode
    ghc
    ghci-completion
    shm ;; structured-haskell-mode
    scion
    ido-ubiquitous
    magit
    markdown-mode
    paredit
    powerline
    smex
    web-mode
    multi-web-mode
    yaml-mode
    elpy
    flymake-cursor
    ;;flymake-less
    less-css-mode
    css-eldoc
    ctags-update
    yasnippet
    solarized-theme
    zenburn-theme)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;;;; Package Init
;; ag.el syntax highlighting
(setq ag-highlight-search t)

(require 'projectile)
(projectile-global-mode)

(ido-mode t)
(ido-ubiquitous-mode t)

;; (require 'auto-complete)
;; (global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'html-mode)

(when (require 'elpy nil t)
  (elpy-enable)
  (elpy-clean-modeline)
  (elpy-use-ipython)
  (defalias 'workon 'pyvenv-workon)
  ;; fixing a key binding bug in elpy
  (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
  ;; fixing another key binding buf in iedit mode
  (define-key global-map (kbd "C-c o") 'iedit-mode)
  (setq elpy-rpc-backend "jedi"))

(require 'flymake-cursor)

;; needed for my emacsclient
;; should probably set this somewhere else
;; (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient")

(eval-after-load "paredit"
  #'(define-key paredit-mode-map (kbd "C-j") 'eval-last-sexp))

(powerline-default-theme)

;; its little brother, for M-x
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;(yas-global-mode 1)

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)


(setq css-indent-offset 2)
;;(require 'flymake-less)
(require 'css-eldoc)

;; multi-web-mode  alternative to web-mode
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;;web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (whitespace-mode 1)
  ;; indentation
  ;(local-set-key (kbd "RET") 'newline-and-indent)
  ;; HTML offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; CSS offset indentation
  (setq web-mode-code-indent-offset 2)
  ;; Script offset indentation (for JavaScript, Java, PHP, etc.)
  (setq web-mode-css-indent-offset 2)
  ;; HTML content indentation
  (setq web-mode-indent-style 2)

  ;; padding
  ;; For <style> parts
  (setq web-mode-style-padding 1)
  ;; For <script> parts
  (setq web-mode-script-padding 1)
  ;; For multi-line blocks
  (setq web-mode-block-padding 0))

(add-hook 'web-mode-hook  'my-web-mode-hook)


;; auto-indent
(electric-indent-mode 1)

;; golang stuff
(setenv "GOPATH" "$HOME/repos/go")
;(require 'go-mode-load)
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "$HOME/repos/go/bin")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)

;; Haskell stuff
(add-to-list 'exec-path "~/.cabal/bin")
;;(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; code alignment
(global-set-key (kbd "C-x a r") 'align-regexp)
;; the below can be used with M-x align-code
;; (add-to-list 'align-rules-list
;;               '(haskell-types
;;                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
;;                 (modes quote (haskell-mode literate-haskell-mode))))
;;  (add-to-list 'align-rules-list
;;               '(haskell-assignment
;;                 (regexp . "\\(\\s-+\\)=\\s-+")
;;                 (modes quote (haskell-mode literate-haskell-mode))))
;;  (add-to-list 'align-rules-list
;;               '(haskell-arrows
;;                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
;;                 (modes quote (haskell-mode literate-haskell-mode))))
;;  (add-to-list 'align-rules-list
;;               '(haskell-left-arrows
;;                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
;;                 (modes quote (haskell-mode literate-haskell-mode))))
