;; -*- mode: emacs-lisp; encoding: utf-8; -*-

;; Run custom first, so that the rest of the initialization process
;; can use what it sets.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-reuse-window t)
 '(column-number-mode t)
 '(company-tooltip-align-annotations t)
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(make-backup-files nil)
 '(menu-bar-mode t)
 '(ns-command-modifier (quote meta))
 '(package-archive-priorities (quote (("gnu" . 2) ("melpa-stable" . 1))))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (json-reformat ecb dockerfile-mode apropospriate-theme zenburn-theme yaml-mode web-mode toml-mode smex smartparens ruby-tools ruby-end racer projectile paredit-menu markdown-mode magit js2-mode ido-ubiquitous hideshowvis haml-mode glsl-mode flycheck es-mode cmake-mode elisp--witness--lisp company cider ag paredit use-package flycheck-rust json-mode)))
 '(projectile-global-mode t)
 '(ring-bell-function (quote ignore))
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values
   (quote
    ((js2-mode-show-strict-warnings)
     (cider-boot-parameters . "cider environ dev repl -s wait")
     (cider-boot-parameters . "cider dev repl -s wait")
     (encoding . utf-8))))
 '(server-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(use-package-always-ensure t)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; package.el initialization; bootstrap to make sure the use-package
;; package is installed.
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Package configurations.

;; Common packages that other stuff is going to want to have present.

(use-package smartparens
  :init
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'ruby-mode-hook 'smartparens-mode))

(use-package company
  :config
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'company-mode)
  (add-hook 'ruby-mode-hook 'company-mode))

(use-package flycheck-rust)

(use-package flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(use-package json-reformat)

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode))

(use-package hideshowvis)

;; Global functionality.

(use-package ag)

(use-package apropospriate-theme)

(use-package ecb
  :commands ecb-activate
  :ensure nil
  :pin melpa)

(use-package ido-ubiquitous)

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-setup-hook 'visual-line-mode))

(use-package paredit-menu)

(use-package projectile)

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; Individual modes / languages.

(use-package cider
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package cmake-mode
  :mode "CMakeLists")

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package glsl-mode)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'smartparens-mode))

(use-package haml-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package json-mode
  :config
  (add-hook 'json-mode-hook 'smartparens-mode)
  (add-hook 'json-mode-hook 'flycheck-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package ruby-end
  :diminish ruby-end-mode)

(use-package ruby-tools
  :diminish ruby-tools-mode)

(use-package racer
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode))

(use-package rust-mode
  :bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common))
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'smartparens-mode))

(use-package toml-mode
  :mode "Cargo\\.lock\\'")

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\.erb\\'" . web-mode)))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; Customize es-mode.
(defun ahw-es-response-reformat-json (status content-type body-buffer)
  (when (and (= 200 status) (string-lessp "application/json" content-type))
    (with-current-buffer body-buffer
      (save-excursion
        (goto-char (point-min))
        ;; (search-forward "\n\n")
        (json-reformat-region (point) (point-max))))))

(defun ahw-es-response-enable-hs-minor-mode (status content-type body-buffer)
  (with-current-buffer body-buffer
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (hs-minor-mode)
    (hideshowvis-minor-mode)
    (hideshowvis-symbols)))

(use-package es-mode
  :config
  (add-to-list 'hs-special-modes-alist '(es-mode "{" "}" "/[*/]" nil))
  (add-to-list 'hs-special-modes-alist '(es-result-mode "{" "}" "/[*/]" nil))

  (add-hook 'es-mode-hook 'hideshowvis-symbols)
  (add-hook 'es-mode-hook 'hideshowvis-minor-mode)
  (add-hook 'es-mode-hook 'hs-minor-mode)
  (add-hook 'es-mode-hook 'smartparens-mode)

  (add-hook 'es-response-success-functions 'ahw-es-response-enable-hs-minor-mode)
  (add-hook 'es-response-success-functions 'ahw-es-response-reformat-json))

;; Hooks for built-in things to built-in things.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(setq ruby-use-smie nil)

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

;; Initialize colors for screen / tmux
(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; Load any .el files in user-lisp.
(dolist (f (directory-files "~/.emacs.d/user-lisp" t "\\.el\\'")) (load f))
