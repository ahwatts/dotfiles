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

(defun ahw-package-install (package)
  "Custom package-install function that only installs a package if it has not
already been installed."
  (unless (package-installed-p package)
    (package-install package)))

;; The melpa.el file is no more; we need to install the package-filter
;; package instead (at least until Emacs 24.4...)

;; ;; Install the melpa-el package, so we can control what comes from
;; ;; MELPA.
;; (unless (package-installed-p 'melpa)
;;   (switch-to-buffer
;;    (url-retrieve-synchronously
;;     "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
;;   (package-install-from-buffer (package-buffer-info) 'single))

(ahw-package-install 'package-filter)
(add-to-list 'package-archive-enable-alist '("melpa" ecb rvm))

;; Create a hook that runs after installing packages that configures
;; packages that were potentially installed in that function.
(defvar ahw-after-installing-packages-hook nil
  "Hook called after ahw-install-packages runs.")

(defun ahw-install-packages ()
  "Function which installs various packages that we want to use."

  ;; Install some packages we want on Emacs 23 only.
  (when (= emacs-major-version 23)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (p '(cl-lib
                 popup))
      (ahw-package-install p)))

  ;; Install some packages we want on Emacs 24 only.
  (when (= emacs-major-version 24)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (p '(ecb
                 ido-ubiquitous))
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
               markdown-mode
               nrepl
               paredit
               paredit-menu
               rinari
               rspec-mode
               ruby-end
               ruby-tools
               rvm
               scss-mode
               smartparens
               smex
               yaml-mode
               yaml-mode
               zenburn-theme))
    (ahw-package-install p))

  (run-hooks 'ahw-after-installing-packages-hook))
(add-hook 'after-init-hook 'ahw-install-packages)

;; Some built-in packages we want available.
(require 'cl)
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
  (set-face-attribute 'default nil :height 100)

  ;; Set the default font.
  (let ((font-name (find-if (lambda (name) (x-list-fonts name))
                            '("Consolas" "Inconsolata" "Bitstream Vera Sans Mono" "DejaVu Sans Mono"))))
    (when font-name
      (set-face-attribute 'default nil :family font-name)))

  ;; Activate Zenburn.
  (defun ahw-turn-on-zenburn ()
    (when window-system
      (require 'color-theme)
      (require 'zenburn-theme)))
  (add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-zenburn))

;; Turn on auto-complete-mode.
(defun ahw-turn-on-auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-auto-complete)

;; Set up Tramp proxies.
(add-to-list 'tramp-default-proxies-alist
             '("\\.reverbnation\\.lan" nil "/ssh:awatts@herbie.reverbnation.com:"))

;; ECB configuration.
(defun ahw-add-ecb-source-paths ()
  (dolist (p (reverse '(("/home/andrew/rubydev/workspace/reverbnation" "reverbnation")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p484@reverbnation/gems" "reverbnation gems")
                        ("/home/andrew/rubydev/workspace/redis_sendmail" "redis_sendmail")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p353@redis_sendmail/gems" "redis_sendmail gems")
                        ("/home/andrew/rubydev/workspace/manticore" "manticore")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p484@manticore/gems" "manticore gems")
                        ("/home/andrew/rubydev/workspace/msmstats" "msmstats")
                        ("/home/andrew/.rvm/gems/ree-1.8.7-2012.02@msmstats/gems" "msmstats gems")
                        ("/home/andrew/rubydev/workspace/rn-chef" "rn-chef")
                        ("/home/andrew/.rvm/gems/ruby-1.9.3-p484@chef-client/gems" "rn-chef gems")
                        ("/home/andrew/rubydev/workspace/nagios-config" "nagios-config")
                        ("/home/andrew/rubydev/workspace/rn-god" "rn-god")
                        ("/home/andrew/rubydev/workspace/god-local" "god-local")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p353@god-local/gems" "god-local gems")
                        ("/home/andrew/rubydev/workspace/apache-upload-progress-module" "apache-upload-progress-module")
                        ("/home/andrew/rubydev/workspace/scripts" "scripts")
                        ("/home/andrew/rpmbuild" "reverb-rpms")
                        ("/home/andrew/.rvm/rubies/ruby-1.9.3-p484/lib/ruby" "ruby-1.9.3-p484")
                        ("/home/andrew/.rvm/rubies/ruby-2.0.0-p353/lib/ruby" "ruby-2.0.0-p353")
                        ("/home/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("/home/andrew/.rvm/gems/ruby-2.0.0-p353@dreamybandnames/gems" "dreamybandnames gems")
                        ("/home/andrew/Projects/euler" "euler")
                        ("/home/andrew/Projects/graphplay" "graphplay")
                        ("/home/andrew/Projects/openhf" "openhf")
                        ("/home/andrew/Projects/songviz" "songviz")
                        ("/home/andrew" "homedir")
                        ("C:/Users/andrew/Projects/redis_sendmail" "redis_sendmail")
                        ("C:/Users/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("C:/Users/andrew/Projects/euler" "euler")
                        ("C:/Users/andrew/Projects/graphplay" "graphplay")
                        ("C:/Users/andrew/Projects/openhf" "openhf")
                        ("C:/Users/andrew/Projects/songviz" "songviz")
                        ("C:/Users/andrew" "homedir")
                        ("/" "/"))))
    (when (file-directory-p (car p))
      (add-to-list 'ecb-source-path p))))

(defun ahw-configure-ecb ()
  (when (package-installed-p 'ecb)
    (require 'ecb)
    (add-hook 'ecb-activate-before-layout-draw-hook 'ahw-add-ecb-source-paths)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-ecb)

;; Elisp configuration.
(defun ahw-configure-elisp-mode ()
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-elisp-mode)

;; Ruby configuration.
(defun ahw-configure-ruby-mode ()
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\`Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\`Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'ruby-end-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-ruby-mode)

;; Javascript configuration.
(defun ahw-turn-on-flymake-jshint ()
  (require 'flymake-jshint)
  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.json$"
                 flymake-jshint-init
                 flymake-simple-cleanup
                 flymake-get-real-file-name)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-flymake-jshint)

;; YAML configuration.
(defun ahw-turn-on-yaml-mode ()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-yaml-mode)

;; Clojure configuration
(defun ahw-configure-clojure-mode ()
  (require 'clojure-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-clojure-mode)

;; Markdown configuration
(defun ahw-turn-on-markdown-mode ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-markdown-mode)

;; CMake configuration.
(defun ahw-turn-on-cmake-mode ()
  (require 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists" . cmake-mode)))
(add-hook 'after-installing-packages-hook 'ahw-turn-on-cmake-mode)

;; Add the paredit menu
(defun ahw-configure-paredit-menu ()
  (require 'paredit-menu))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-paredit-menu)

;; Initialize colors for screen / tmux
(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; Bind the F11 key to fullscreenize Emacs.
(defvar ahw-prev-fullscreen
  (frame-parameter nil 'fullscreen)
  "Stores the value of the fullscreen frame parameter before going to fullscreen.")

(defun ahw-toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (let ((fscr (frame-parameter nil 'fullscreen)))
      (if (eq fscr 'fullboth)
          (progn
            (set-frame-parameter nil 'fullscreen ahw-prev-fullscreen)
            (menu-bar-mode))
        (setq ahw-prev-fullscreen fscr)
        (set-frame-parameter nil 'fullscreen 'fullboth)
        (menu-bar-mode -1)))))

(global-set-key [f11] 'ahw-toggle-fullscreen)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-offsets-alist (quote ((template-args-cont . +))))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(gdb-many-windows t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode (window-system))
 '(mouse-yank-at-point t)
 '(nrepl-popup-stacktraces nil)
 '(rspec-key-command-prefix "s")
 '(rspec-use-opts-file-when-available nil)
 '(rspec-use-rvm t)
 '(ruby-deep-indent-paren (quote (t)))
 '(rvm-configuration-file-name "/home/andrew/.rvmrc")
 '(safe-local-variable-values (quote ((encoding . binary) (encoding . utf-8) (whitespace-line-column . 80) (lexical-binding . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scss-compile-at-save nil)
 '(semantic-mode t)
 '(show-paren-mode t)
 '(smartparens-global-mode (= emacs-major-version 24))
 '(smex-save-file "~/.emacs.d/smex-items")
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode emacs-lisp-mode clojure-mode lisp-interaction-mode)))
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
