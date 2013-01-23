;; -*- mode: emacs-lisp -*-

;; Start the server.
(server-mode)

(setq package-available-p (>= emacs-major-version 24))

;; Package configuration.
(when package-available-p
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (dolist (p '(starter-kit
               starter-kit-bindings
               starter-kit-eshell
               starter-kit-js
               starter-kit-lisp
               starter-kit-ruby
               auto-complete
               ecb-snapshot
               color-theme
               zenburn-theme
               coffee-mode
               haml-mode
               scss-mode
               yaml-mode
               clojure-mode
               nrepl
               ac-nrepl
               ruby-tools
               ruby-end
               flymake-ruby
               flymake-jshint
               yaml-mode))
    (when (not (package-installed-p p))
      (package-install p))))

(require 'cl)
(when window-system
  (progn
    ;; Set the font size. Maybe this should depend somehow on what
    ;; machine we're on?
    (set-face-attribute 'default nil :height 110)

    ;; Set the default font.
    (let ((font-name (find-if (lambda (name) (x-list-fonts name))
                              '("Consolas" "Inconsolata" "Bitstream Vera Sans Mono" "DejaVu Sans Mono"))))
      (when font-name
        (set-face-attribute 'default nil :family font-name)))

    ;; Activate Zenburn.
    (when package-available-p
      (defun ahw-turn-on-zenburn ()
        (require 'color-theme)
        (require 'zenburn-theme))
      (add-hook 'after-init-hook 'ahw-turn-on-zenburn))))

;; Turn on auto-complete-mode.
(when package-available-p
  (defun ahw-turn-on-auto-complete ()
    (require 'auto-complete-config)
    (ac-config-default))
  (add-hook 'after-init-hook 'ahw-turn-on-auto-complete))

;; Set up Tramp proxies.
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '("\\.reverbnation\\.lan" nil "/ssh:awatts@angus.reverbnation.com:"))

;; ECB configuration.
(when package-available-p
  (setq stack-trace-on-error t)
  (defun ahw-add-ecb-source-paths ()
    (require 'ecb-snapshot-autoloads)
    (dolist (p (reverse '(("/home/andrew/rubydev/workspace/reverbnation" "reverbnation")
                          ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@reverbnation/gems" "reverbnation gems")
                          ("/home/andrew/.rvm/gems/ruby-1.9.3-p374@reverbnation/gems" "reverbnation-2.3 gems")
                          ("/home/andrew/rubydev/workspace/manticore" "manticore")
                          ("/home/andrew/.rvm/gems/ruby-1.9.3-p374@manticore/gems" "manticore gems")
                          ("/home/andrew/rubydev/workspace/msmstats" "msmstats")
                          ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@msmstats/gems" "msmstats gems")
                          ("/home/andrew/rubydev/workspace/rn-chef" "rn-chef")
                          ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@chef_client/gems" "rn-chef gems")
                          ("/home/andrew/rubydev/workspace/nagios-config" "nagios-config")
                          ("/home/andrew/rubydev/workspace/rn-god" "rn-god")
                          ("/home/andrew/rubydev/workspace/apache-upload-progress-module" "apache-upload-progress-module")
                          ("/home/andrew/rubydev/workspace/scripts" "scripts")
                          ("/home/andrew/rpmbuild" "reverb-rpms")
                          ("C:/Users/andrew/Projects/dbn" "dreamybandnames")
                          ("C:/Users/andrew/Documents/Projects/euler" "euler")
                          ("C:/Users/andrew/Projects/euler" "euler")
                          ("/home/andrew/Projects/euler" "euler")
                          ("C:/Users/andrew" "homedir")
                          ("/home/andrew" "homedir")
                          ("/" "/"))))
      (when (file-directory-p (car p))
        (add-to-list 'ecb-source-path p))))
  (add-hook 'ecb-activate-before-layout-draw-hook 'ahw-add-ecb-source-paths))

;; Ruby configuration.
(when package-available-p
  (require 'inf-ruby)
  (add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'ruby-end-mode)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

;; Javascript configuration.
(when package-available-p
  (defun ahw-turn-on-flymake-jshint ()
    (require 'flymake-jshint)
    (add-to-list 'flymake-allowed-file-name-masks
                 '(".+\\.json$"
                   flymake-jshint-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)))
  (add-hook 'after-init-hook 'ahw-turn-on-flymake-jshint))

;; YAML configuration.
(when package-available-p
  (defun ahw-turn-on-yaml-mode ()
    (require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
  (add-hook 'after-init-hook 'ahw-turn-on-yaml-mode))

;; DNS configuration
(when package-available-p
  (add-hook 'dns-mode-hook 'flyspell-mode-off))

;; Clojure configuration
(when package-available-p
  (add-hook 'clojure-mode-hook 'eldoc-mode))

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(nrepl-popup-stacktraces nil)
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values (quote ((encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Maximize on Windows.
(when (string= "w32" window-system)
  (w32-send-sys-command 61488))
