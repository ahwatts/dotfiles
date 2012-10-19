;; -*- mode: emacs-lisp -*-

;; Start the server.
(server-mode)

;; Package configuration.
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
             ruby-end))
  (when (not (package-installed-p p))
    (package-install p)))

;; Set the default font.
(set-face-attribute 'default nil :height 110)
(require 'cl)
(let ((font-name (find-if (lambda (name) (x-list-fonts name))
                          '("Consolas" "Inconsolata" "Bitstream Vera Sans Mono" "DejaVu Sans Mono"))))
  (when font-name
    (set-face-attribute 'default nil :family font-name)))

;; Activate Zenburn.
(defun ahw-turn-on-zenburn ()
  (require 'color-theme)
  (require 'zenburn-theme))
(add-hook 'after-init-hook 'ahw-turn-on-zenburn)

;; Turn on auto-complete-mode.
(defun ahw-turn-on-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))
(add-hook 'after-init-hook 'ahw-turn-on-auto-complete)

;; Set up Tramp proxies.
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
	     '("\\.reverbnation\\.lan" nil "/ssh:awatts@angus.reverbnation.com:"))

;; ECB configuration.
(setq stack-trace-on-error t)
(defun ahw-add-ecb-source-paths ()
  (require 'ecb-snapshot-autoloads)
  (dolist (p (reverse '(("/home/andrew/rubydev/workspace/reverbnation" "reverbnation")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@reverbnation/gems" "reverbnation gems")
                        ("/home/andrew/rubydev/workspace/manticore" "manticore")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p194@manticore/gems" "manticore gems")
                        ("/home/andrew/rubydev/workspace/msmstats" "msmstats")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@msmstats/gems" "msmstats gems")
                        ("/home/andrew/rubydev/workspace/rn-chef" "rn-chef")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@chef_client/gems" "rn-chef gems")
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
(add-hook 'ecb-activate-before-layout-draw-hook 'ahw-add-ecb-source-paths)

;; Ruby configuration.
(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'ruby-end-mode)

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
 '(nrepl-popup-stacktraces nil)
 '(ruby-deep-indent-paren (quote (t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Maximize on Windows.
(when (string= "w32" window-system)
  (w32-send-sys-command 61488))
