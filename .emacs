;; -*- mode: emacs-lisp -*-

;; External packages this file supports (but doesn't automatically install):
;; company
;; paredit
;; rainbow-delimiters
;; smex

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

(defun ahw-package-installed-p (pkg)
  (and (fboundp 'package-installed-p)
       (package-installed-p pkg)))

;; Elisp (and ielm) configuration.
(defun ahw-configure-elisp-like-mode (mode-hook)
  (add-hook mode-hook 'eldoc-mode)
  (when (ahw-package-installed-p 'paredit)
    (add-hook mode-hook 'paredit-mode))
  (when (ahw-package-installed-p 'rainbow-delimiters)
    (add-hook mode-hook 'rainbow-delimiters-mode))
  (when (ahw-package-installed-p 'company)
    (add-hook mode-hook 'company-mode)))

(defun ahw-configure-elisp-mode ()
  (ahw-configure-elisp-like-mode 'emacs-lisp-mode-hook))
(add-hook 'after-init-hook 'ahw-configure-elisp-mode)

(defun ahw-configure-ielm-mode ()
  (ahw-configure-elisp-like-mode 'ielm-mode-hook))
(add-hook 'after-init-hook 'ahw-configure-ielm-mode)

;; Smex configuration.
(defun ahw-configure-smex ()
  (when (ahw-package-installed-p 'smex)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))
(add-hook 'after-init-hook 'ahw-configure-smex)

;; Set up our local user-lisp path, and force anything in there to get
;; loaded now.
(add-to-list 'load-path "~/.emacs.d/user-lisp")
(dolist (f (directory-files "~/.emacs.d/user-lisp" t "\\.el\\'")) (load f))

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
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
