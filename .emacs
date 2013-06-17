;; -*- mode: emacs-lisp -*-

;; Start the server.
(server-mode)

(setq package-available-p (>= emacs-major-version 24))

;; Make sure package.el is installed.
(unless (require 'package nil t)
  (let ((package-el-path "~/.emacs.d/package"))
    (when (and (= emacs-major-version 23)
               (not (file-readable-p (concat package-el-path "/package.el"))))
      (make-directory "~/.emacs.d/package" t)
      (url-copy-file "http://bit.ly/pkg-el23" "~/.emacs.d/package/package.el" t))
    (add-to-list 'load-path package-el-path)
    (require 'package)))

;; package.el configuration.
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Install the melpa-el package, so we can control what comes from
;; MELPA.
(unless (package-installed-p 'melpa)
  (switch-to-buffer
   (url-retrieve-synchronously
    "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
  (package-install-from-buffer (package-buffer-info) 'single))
(add-to-list 'package-archive-enable-alist '("melpa" . 'ecb))

;; Install some packages we want.
(unless package-archive-contents (package-refresh-contents))
(dolist (p '(starter-kit
             starter-kit-bindings
             starter-kit-eshell
             starter-kit-js
             starter-kit-lisp
             starter-kit-ruby
             auto-complete
             ecb
             color-theme
             zenburn-theme
             coffee-mode
             haml-mode
             scss-mode
             yaml-mode
             clojure-mode
             nrepl
             ac-nrepl
             rinari
             ruby-tools
             ruby-end
             flymake-ruby
             flymake-jshint
             yaml-mode
             markdown-mode))
  (unless (package-installed-p p) (package-install p)))

(require 'cl)
(when window-system
  ;; Set the font size. Maybe this should depend somehow on what
  ;; machine we're on?
  (set-face-attribute 'default nil :height 110)

  ;; Set the default font.
  (let ((font-name (find-if (lambda (name) (x-list-fonts name))
                            '("Consolas" "Inconsolata" "Bitstream Vera Sans Mono" "DejaVu Sans Mono"))))
    (when font-name
      (set-face-attribute 'default nil :family font-name)))

  ;; Activate Zenburn.
  (defun ahw-turn-on-zenburn ()
    (require 'color-theme)
    (require 'zenburn-theme))
  (add-hook 'after-init-hook 'ahw-turn-on-zenburn))

;; Turn on auto-complete-mode.
(defun ahw-turn-on-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))
(add-hook 'after-init-hook 'ahw-turn-on-auto-complete)

;; Set up Tramp proxies.
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '("\\.reverbnation\\.lan" nil "/ssh:awatts@herbie.reverbnation.com:"))

;; ECB configuration.
(setq stack-trace-on-error t)
(defun ahw-add-ecb-source-paths ()
  (require 'ecb-autoloads)
  (dolist (p (reverse '(("/home/andrew/rubydev/workspace/reverbnation" "reverbnation")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p429@reverbnation/gems" "reverbnation gems")
                        ("/home/andrew/rubydev/workspace/manticore" "manticore")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p429@manticore/gems" "manticore gems")
                        ("/home/andrew/rubydev/workspace/msmstats" "msmstats")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@msmstats/gems" "msmstats gems")
                        ("/home/andrew/rubydev/workspace/rn-chef" "rn-chef")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p429@chef-client/gems" "rn-chef gems")
                        ("/home/andrew/rubydev/workspace/nagios-config" "nagios-config")
                        ("/home/andrew/rubydev/workspace/rn-god" "rn-god")
                        ("/home/andrew/rubydev/workspace/god-local" "god-local")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p195@god-local/gems" "god-local gems")
                        ("/home/andrew/rubydev/workspace/apache-upload-progress-module" "apache-upload-progress-module")
                        ("/home/andrew/rubydev/workspace/scripts" "scripts")
                        ("/home/andrew/rpmbuild" "reverb-rpms")
                        ("/home/andrew/.rvm/rubies/ruby-1.9.3-p429/lib/ruby" "ruby-1.9.3-p429")
                        ("/home/andrew/.rvm/rubies/ruby-2.0.0-p195/lib/ruby" "ruby-2.0.0-p195")
                        ("/home/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("/home/andrew/Projects/euler" "euler")
                        ("/home/andrew/Projects/graphplay" "graphplay")
                        ("/home/andrew/Projects/openhf" "openhf")
                        ("/home/andrew" "homedir")
                        ("C:/Users/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("C:/Users/andrew/Projects/euler" "euler")
                        ("C:/Users/andrew/Projects/graphplay" "graphplay")
                        ("C:/Users/andrew/Projects/openhf" "openhf")
                        ("C:/Users/andrew/Projects/songviz" "songviz")
                        ("C:/Users/andrew" "homedir")
                        ("/" "/"))))
    (when (file-directory-p (car p))
      (add-to-list 'ecb-source-path p))))
(add-hook 'ecb-activate-before-layout-draw-hook 'ahw-add-ecb-source-paths)

;; Ruby configuration.
(require 'inf-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-end-mode)

;; Javascript configuration.
(defun ahw-turn-on-flymake-jshint ()
  (require 'flymake-jshint)
  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.json$"
                 flymake-jshint-init
                 flymake-simple-cleanup
                 flymake-get-real-file-name)))
(add-hook 'after-init-hook 'ahw-turn-on-flymake-jshint)

;; YAML configuration.
(defun ahw-turn-on-yaml-mode ()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(add-hook 'after-init-hook 'ahw-turn-on-yaml-mode)

;; DNS configuration
(add-hook 'dns-mode-hook 'flyspell-mode-off)

;; Clojure configuration
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; Markdown configuration
(defun ahw-turn-on-markdown-mode ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(add-hook 'after-init-hook 'ahw-turn-on-markdown-mode)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(nrepl-popup-stacktraces nil)
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values (quote ((encoding . binary) (encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t))))
 '(scss-compile-at-save nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Maximize on Windows.
(when (string= "w32" window-system)
  (w32-send-sys-command 61488))
