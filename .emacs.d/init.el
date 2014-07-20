
;; loading interface defaults ASAP
(load (concat user-emacs-directory "init/interface.el"))

;; init packages
(load (concat user-emacs-directory "init/init-packages.el"))

;; function definitions
(load (concat user-emacs-directory "init/functions.el"))

;; load other emacs defaults
(load (concat user-emacs-directory "init/defaults.el"))

;; language hooks
(load (concat user-emacs-directory "init/languages.el"))

;; load system-tailored defaults you should probably edit this
(load (concat user-emacs-directory "init/system.el"))

;; Load init files
;; (mapcar
;;  (lambda (f) (load-file f))
;;  (file-expand-wildcards "~/.emacs.d/init/*.el"))

;; emacs server goodness
;;(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(magit-auto-revert-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
