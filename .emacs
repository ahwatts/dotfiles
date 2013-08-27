;; -*- mode: emacs-lisp -*-

;; Start the server.
(server-mode)

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

(defun ahw-package-install (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun ahw-install-packages ()
  ;; Install some packages we want on Emacs 23 only.
  (when (= emacs-major-version 23)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (p '(cl-lib
		 popup))
      (ahw-package-install p)))

  ;; Install some packages we want on Emacs 24 only.
  (when (= emacs-major-version 24)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (p '(ecb))
      (ahw-package-install p)))

  ;; Install some packages we want on all Emacs versions.
  (unless package-archive-contents (package-refresh-contents))
  (dolist (p '(ac-nrepl
	       auto-complete
	       clojure-mode
	       cmake-mode
	       coffee-mode
	       color-theme
	       flymake-jshint
	       flymake-ruby
	       haml-mode
	       ido-ubiquitous
	       markdown-mode
	       nrepl
	       paredit
	       paredit-menu
	       rinari
	       ruby-end
	       ruby-tools
	       scss-mode
	       smex
	       yaml-mode
	       yaml-mode
	       zenburn-theme))
    (ahw-package-install p)))
(add-hook 'after-init-hook 'ahw-install-packages)

;; Some built-in packages we want available.
(require 'cl)
(require 'inf-ruby)
(require 'saveplace)
(require 'tramp)
(require 'uniquify)

;; Keybindings changes.

;; ibuffer is nicer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; I'm not sure what hippie-expand is, but I'll try it for now.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Swap out the regular isearches for their regexp counterparts.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; use smex.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is the pre-smex M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Things for non-console Emacs.
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
(add-to-list 'tramp-default-proxies-alist
             '("\\.reverbnation\\.lan" nil "/ssh:awatts@herbie.reverbnation.com:"))

;; ECB configuration.
(setq stack-trace-on-error t)
(defun ahw-add-ecb-source-paths ()
  (require 'ecb-autoloads)
  (dolist (p (reverse '(("/home/andrew/rubydev/workspace/reverbnation" "reverbnation")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p448@reverbnation/gems" "reverbnation gems")
                        ("/home/andrew/rubydev/workspace/manticore" "manticore")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p448@manticore/gems" "manticore gems")
                        ("/home/andrew/rubydev/workspace/msmstats" "msmstats")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@msmstats/gems" "msmstats gems")
                        ("/home/andrew/rubydev/workspace/rn-chef" "rn-chef")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p448@chef-client/gems" "rn-chef gems")
                        ("/home/andrew/rubydev/workspace/nagios-config" "nagios-config")
                        ("/home/andrew/rubydev/workspace/rn-god" "rn-god")
                        ("/home/andrew/rubydev/workspace/god-local" "god-local")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p247@god-local/gems" "god-local gems")
                        ("/home/andrew/rubydev/workspace/apache-upload-progress-module" "apache-upload-progress-module")
                        ("/home/andrew/rubydev/workspace/scripts" "scripts")
                        ("/home/andrew/rpmbuild" "reverb-rpms")
                        ("/home/andrew/.rvm/rubies/ruby-1.9.3-p448/lib/ruby" "ruby-1.9.3-p448")
                        ("/home/andrew/.rvm/rubies/ruby-2.0.0-p247/lib/ruby" "ruby-2.0.0-p247")
                        ("/home/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p247@dreamybandnames/gems" "dreamybandnames gems")
                        ("/home/andrew/Projects/euler" "euler")
                        ("/home/andrew/Projects/graphplay" "graphplay")
                        ("/home/andrew/Projects/openhf" "openhf")
                        ("/home/andrew/Projects/songviz" "songviz")
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

;; Elisp configuration.
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Ruby configuration.
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
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
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

;; Markdown configuration
(defun ahw-turn-on-markdown-mode ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(add-hook 'after-init-hook 'ahw-turn-on-markdown-mode)

;; CMake configuration.
(defun ahw-turn-on-cmake-mode ()
  (require 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists" . cmake-mode)))
(add-hook 'after-init-hook 'ahw-turn-on-cmake-mode)

;; Initialize colors for screen / tmux
(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; Add the paredit menu
(require 'paredit-menu nil t)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mouse-yank-at-point t)
 '(nrepl-popup-stacktraces nil)
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values (quote ((encoding . binary) (encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scss-compile-at-save nil)
 '(show-paren-mode t)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Maximize on Windows.
(when (string= "w32" window-system)
  (w32-send-sys-command 61488))
