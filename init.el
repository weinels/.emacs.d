(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
(setq my-packages
      '(auto-compile auto-complete cl-lib ctable deferred el-get emacs-async epc fuzzy helm jedi packed popup python-environment yasnippet yasnippet-snippets))
      
(el-get 'sync my-packages)

;; use helm for some common tasks
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;; enable yas snippets
(yas-global-mode 1)

;; programming language hooks
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'jedi:setup)

;; add .tpl files to latex mode
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . latex-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-candidate-number ((t nil)))
 '(helm-selection ((t (:background "yellow" :foreground "black"))))
 '(mode-line ((t (:background "grey90" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey75" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(region ((t (:background "lightgoldenrod2" :foreground "black")))))
