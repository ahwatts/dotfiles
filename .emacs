;; -*- mode: emacs-lisp -*-

;; External packages this file supports (but doesn't automatically install):
;; paredit
;; rainbow-delimiters
;; company

;; built-in packages we want available.
(require 'uniquify)

;; ibuffer is nicer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Swap out the regular isearches for their regexp counterparts.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Something I do way too often.
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; Elisp configuration.
(defun ahw-configure-elisp-mode ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (when (package-installed-p 'paredit)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  (when (package-installed-p 'rainbow-delimiters)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
  (when (package-installed-p 'company)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)))
(add-hook 'after-init-hook 'ahw-configure-elisp-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(diff-switches "-u")
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
