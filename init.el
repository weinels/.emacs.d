(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; el-get installed packages.
;; to get list:
;; M-x ielm
;; `(setq my-packages
;;              ',(mapcar #'el-get-as-symbol
;;                        (el-get-list-package-names-with-status "installed")))

(setq my-packages
      '(auto-complete cl-lib ctable deferred el-get emacs-async epc fuzzy helm jedi package packed popup python-environment rainbow-mode yasnippet yasnippet-snippets exec-path-from-shell))
      
(el-get 'sync my-packages)

;; ITERM2 MOUSE SUPPORT
(unless window-system
      (require 'mouse)
      (xterm-mouse-mode t)
      (global-set-key [mouse-4] (lambda ()
				  (interactive)
				  (scroll-down 1)))
      (global-set-key [mouse-5] (lambda ()
				  (interactive)
				  (scroll-up 1)))
      (defun track-mouse (e)) 
      (setq mouse-sel-mode t))

;; use helm for some common tasks
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; set shortcut for listing python methods
(global-set-key (kbd "C-x m") #'helm-occur)

;; since helm is installed, no need for a list buffers command
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x b") #'helm-buffers-list)

;; enable yas snippets
(yas-global-mode 1)

;; display line numbers in program mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; spell check strings and comments in program mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; python hooks
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

;; add .tpl files to latex mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . latex-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-semantic-idle-scheduler-mode nil)
 '(helm-mode t)
 '(package-selected-packages (quote (rainbow-mode)))
 '(scroll-error-top-bottom t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:foreground "color-27"))))
 '(font-lock-comment-face ((t (:foreground "brightyellow" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "color-123"))))
 '(font-lock-function-name-face ((t (:foreground "color-208"))))
 '(font-lock-keyword-face ((t (:foreground "color-201"))))
 '(font-lock-string-face ((t (:foreground "brightyellow"))))
 '(font-lock-type-face ((t (:foreground "color-40"))))
 '(font-lock-variable-name-face ((t (:foreground "color-197"))))
 '(helm-candidate-number ((t nil)))
 '(helm-selection ((t (:background "yellow" :foreground "black"))))
 '(line-number ((t (:foreground "color-250"))))
 '(mode-line ((t (:background "grey90" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey75" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(region ((t (:background "lightgoldenrod2" :foreground "black")))))
