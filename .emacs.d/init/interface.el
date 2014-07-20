;; Hide all GUI stuff. Emacs is no GUI!
(defun intinig-no-gui ()
  "Turns off extra-gui stuff"
  ;; (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  ;;   (when (fboundp mode) (funcall mode -1))))
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))


(intinig-no-gui)
(add-hook 'before-make-frame-hook 'intinig-no-gui)

;; sets the default font to menlo
;;(defun fontify-frame (frame)
;;  (set-frame-parameter frame 'font "Menlo Regular-12"))
;;(fontify-frame nil)
;;(push 'fontify-frame after-make-frame-functions)

(set-default-font "Ubuntu Mono 11")

;; default window size
(setq default-frame-alist '((width . 120) (height . 50) ))

;; no strange gnu guy
(setq inhibit-startup-message t)
