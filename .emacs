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
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea"
     "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c"
     default)))
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.50")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flycheck-javascript-flow-args nil)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(lsp-enable-snippet nil)
 '(lsp-prefer-flymake nil)
 '(magit-diff-arguments (quote ("--ignore-all-space" "--no-ext-diff" "--stat")))
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
    (ag
     apropospriate-theme
     cider
     cmake-mode
     company
     company-go
     company-lsp
     direnv
     dockerfile-mode
     ecb
     ein
     elisp--witness--lisp
     enh-ruby-mode
     es-mode
     flycheck
     flycheck-flow
     flycheck-gometalinter
     flycheck-rust
     glsl-mode
     go-mode
     groovy-mode
     haml-mode
     ido-completing-read+
     js2-mode
     json-mode
     json-reformat
     lsp-mode
     lsp-rust
     lsp-ui
     magit
     markdown-mode
     org
     paredit
     paredit-menu
     projectile
     protobuf-mode
     racer
     ripgrep
     rpm-spec-mode
     ruby-end
     ruby-hash-syntax
     ruby-tools
     rust-mode
     smartparens
     smex
     toml-mode
     use-package
     web-mode
     yaml-mode
     zenburn-theme)))
 '(projectile-global-mode t)
 '(ring-bell-function (quote ignore))
 '(ripgrep-arguments (quote ("--sort-files")))
 '(ruby-deep-indent-paren (quote (t)))
 '(server-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(use-package-always-ensure t)
 '(visible-bell nil)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2))
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
(setq use-package-compute-statistics 1)

;; Customize es-mode.
(defun ahw-es-response-reformat-json (status content-type body-buffer)
  "(STATUS CONTENT-TYPE BODY-BUFFER) Pretty-print ES JSON responses."
  (when (and (= 200 status) (string-lessp "application/json" content-type))
    (with-current-buffer body-buffer
      (save-excursion
        (goto-char (point-min))
        ;; (search-forward "\n\n")
        (json-reformat-region (point) (point-max))))))

(defun ahw-es-response-enable-hs-minor-mode (status content-type body-buffer)
  "(STATUS CONTENT-TYPE BODY-BUFFER) Enables hideshow / hideshowvis on ES response buffers."
  (with-current-buffer body-buffer
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (hs-minor-mode)
    ;; (hideshowvis-minor-mode)
    ;; (hideshowvis-symbols)
    ))

;; Package configurations.

(use-package ag)

(use-package apropospriate-theme)

(use-package cider
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode))
  :after (paredit))

(use-package clojure-mode
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . eldoc-mode))
  :after (paredit))

(use-package cmake-mode
  :mode "CMakeLists")

(use-package company
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :init (push 'company-lsp company-backends)
  :after (company lsp-mode))

(use-package dockerfile-mode
  :mode "\\`Dockerfile")

(use-package direnv)

(use-package ecb
  :commands ecb-activate
  :ensure nil
  :pin melpa)

(use-package ein)

(use-package es-mode
  :hook ((es-mode . hideshowvis-symbols)
         (es-mode . hideshowvis-minor-mode)
         (es-mode . hs-minor-mode)
         (es-mode . smartparens-mode)
         (es-response-success-functions . ahw-es-response-enable-hs-minor-mode)
         (es-response-success-functions . ahw-es-response-reformat-json))
  :config
  (add-to-list 'hs-special-modes-alist '(es-mode "{" "}" "/[*/]" nil))
  (add-to-list 'hs-special-modes-alist '(es-result-mode "{" "}" "/[*/]" nil)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :pin melpa)

(use-package flycheck-flow
  :after (flycheck))

;; (use-package flycheck-rust
;;   :init (with-eval-after-load 'rust-mode
;;           (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
;;   :after (rust flycheck))

(use-package flycheck-gometalinter
  :config (flycheck-gometalinter-setup))

(use-package glsl-mode)

(use-package go-mode)

(use-package groovy-mode)

(use-package haml-mode)

(use-package ido-completing-read+)

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))

(use-package json-mode
  :pin melpa)

(use-package json-reformat)

(use-package lsp-mode
  :hook (rust-mode . lsp))

;; (use-package lsp-rust
;;   :hook (rust-mode . lsp-rust-enable)
;;   :after (lsp-mode rust-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after (lsp-mode))

(use-package magit
  :commands magit-status
  :bind     ("C-x g" . magit-status)
  :hook     (git-commit-setup . visual-line-mode)
  :config   (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill))

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . visual-line-mode))

(use-package org)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (ielm-mode . paredit-mode)))

(use-package protobuf-mode)

(use-package paredit-menu)

(use-package projectile
  :config      (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package ripgrep)

(use-package rpm-spec-mode)

(use-package enh-ruby-mode
  :mode (("\\.rb\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :config (add-hook 'enh-ruby-mode-hook (lambda () (local-unset-key (kbd "C-c /"))))
  :hook ((enh-ruby-mode . ruby-tools-mode)))

(use-package ruby-end
  :diminish ruby-end-mode)

(use-package ruby-tools
  :diminish ruby-tools-mode)

(use-package ruby-hash-syntax)

(use-package rust-mode)

(use-package toml-mode
  :mode "Cargo\\.lock\\'")

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\.erb\\'" . web-mode)))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; (require 'lsp)
;; (require 'lsp-rust)

;; No customize option for this?
(setq ruby-use-smie nil)

;; Can't set this in customize because I want to change it later in
;; the process, but then when we load a custom theme, any subsequent
;; modifications are clobbered.
(setq safe-local-variable-values '((encoding . utf-8)))

;; Hooks for built-in things to built-in things.
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
