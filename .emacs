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
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://hiddencameras.milkbox.net/packages/"))
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

;; (ahw-package-install 'package-filter)
;; (add-to-list 'package-archive-enable-alist '("melpa" ecb es-mode rhtml-mode rvm))

;; Create a hook that runs after installing packages that configures
;; packages that were potentially installed in that function.
(defvar ahw-after-installing-packages-hook nil
  "Hook called after ahw-install-packages runs.")

;; ;; Maximize on Windows. Do this last, after all the other things in
;; ;; the after-installing-packages-hook happens (therefore, add it to
;; ;; the hook first. Obviously.)
;; (defun ahw-maximize-window ()
;;   (when (string= "w32" window-system)
;;     (w32-send-sys-command 61488)))
;; (add-hook 'ahw-after-installing-packages-hook 'ahw-maximize-window)

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
                 ido-ubiquitous
                 smex))
      (ahw-package-install p)))

  ;; Install some packages we want on all Emacs versions.
  (unless package-archive-contents (package-refresh-contents))
  (dolist (p '(ack-and-a-half
               ;; auto-complete
               cider
               clojure-mode
               cmake-mode
               coffee-mode
               color-theme
               company
               dockerfile-mode
               es-mode
               flx-ido
               flycheck
               glsl-mode
               haml-mode
               hideshowvis
               js2-mode
               json-reformat
               json-mode
               magit
               markdown-mode
               paredit
               paredit-menu
               projectile
               projectile-rails
               rhtml-mode
               rinari
               robe
               rspec-mode
               ruby-end
               ruby-tools
               rust-mode
               rvm
               scss-mode
               smartparens
               toml-mode
               yaml-mode
               yaml-mode
               zenburn-theme))
    (ahw-package-install p))

  (run-hooks 'ahw-after-installing-packages-hook))
(add-hook 'after-init-hook 'ahw-install-packages)

;; Some built-in packages we want available.
(require 'cl-lib)
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

;; Org-mode shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Something I do way too often.
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; Random Ruby stuff
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; Magit key bindings
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame-mode)

;; use smex.
(when (package-installed-p 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)

  ;; This is the pre-smex M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Things for non-console Emacs.
(when window-system
  ;; Set the font size. Maybe this should depend somehow on what
  ;; machine we're on?
  (set-face-attribute 'default nil :height 100)

  ;; Set the default font.
  (let ((font-name (cl-find-if (lambda (name) (x-list-fonts name))
                               '("Consolas" "Inconsolata" "Bitstream Vera Sans Mono" "DejaVu Sans Mono"))))
    (when font-name
      (set-face-attribute 'default nil :family font-name))))

(defun ahw-increase-font-height ()
  (interactive)
  (let* ((face-attrs (face-all-attributes 'default))
         (size (cdr (assoc :height face-attrs))))
    (set-face-attribute 'default nil :height (+ size 10))))

(defun ahw-decrease-font-height ()
  (interactive)
  (let* ((face-attrs (face-all-attributes 'default))
         (size (cdr (assoc :height face-attrs))))
    (set-face-attribute 'default nil :height (- size 10))))

(global-set-key (kbd "M-]") 'ahw-increase-font-height)
(global-set-key (kbd "M-[") 'ahw-decrease-font-height)

;; Ack and a half config.
(defun ahw-configure-ack-and-a-half ()
  (require 'ack-and-a-half)
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-ack-and-a-half)

;; Activate Zenburn if we're in a windowing system or a 256-color terminal.
(defun ahw-turn-on-zenburn ()
  (when (or window-system
            (> (length (tty-color-alist)) 8))
    (require 'color-theme)
    (require 'zenburn-theme)
    ;; Apparently this doesn't get reset when we switch to zenburn.
    (when (package-installed-p 'cider)
      (setq cider-stacktrace-frames-background-color (cider-scale-background-color)))))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-zenburn)

;; ;; Turn on auto-complete-mode.
;; (defun ahw-turn-on-auto-complete ()
;;   (require 'auto-complete-config)
;;   (ac-config-default))
;; (add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-auto-complete)

;; Set up Tramp proxies.
(add-to-list 'tramp-default-proxies-alist
             '("\\.reverbnation\\.lan" nil "/ssh:awatts@herbie.reverbnation.com:"))

;; ECB configuration.
(defun ahw-add-ecb-source-paths ()
  (dolist (p (reverse '(("/home/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("/home/andrew/.rvm/gems/ruby-2.1.2@dreamybandnames/gems" "dreamybandnames gems")
                        ("/home/andrew/Projects/openhf" "openhf")
                        ("/home/andrew/.rvm/gems/ruby-2.1.2@openhf/gems" "openhf gems")
                        ("/home/andrew/Projects/songviz" "songviz")
                        ("/home/andrew/.rvm/gems/ruby-2.1.2@songviz/gems" "songviz gems")
                        ("/home/andrew/.rvm/src/ruby-1.9.3-p551" "ruby-1.9.3-p551")
                        ("/home/andrew/.rvm/rubies/ruby-1.9.3-p551/lib/ruby" "ruby-1.9.3-p551 (stdlib)")
                        ("/home/andrew/.rvm/src/ruby-2.0.0-p598" "ruby-2.0.0-p598")
                        ("/home/andrew/.rvm/rubies/ruby-2.0.0-p598/lib/ruby" "ruby-2.0.0-p598 (stdlib)")
                        ("/home/andrew/.rvm/src/ruby-2.1.5" "ruby-2.1.5")
                        ("/home/andrew/.rvm/rubies/ruby-2.1.5/lib/ruby" "ruby-2.1.5 (stdlib)")
                        ("/home/andrew/Projects/euler" "euler")
                        ("/home/andrew/Projects/graphplay" "graphplay")
                        ("/home/andrew/Projects/graphplay-rs" "graphplay-rs")
                        ("/home/andrew/Projects/hub-pics" "hub-pics")
                        ("/home/andrew" "homedir")

                        ("/home/awatts/rubydev/workspace/reverbnation" "reverbnation")
                        ("/home/awatts/.rvm/gems/ruby-1.9.3-p550@reverbnation/gems" "reverbnation gems")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@reverbnation/gems" "reverbnation gems (2.1)")
                        ("/home/awatts/rubydev/workspace/redis_sendmail" "redis_sendmail")
                        ("/home/awatts/.rvm/gems/ruby-2.0.0-p598@redis_sendmail/gems" "redis_sendmail gems")
                        ("/home/awatts/rubydev/workspace/rn_image_server" "rn_image_server")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@rn_image_server/gems" "rn_image_server gems")
                        ("/home/awatts/rubydev/workspace/rn-chef" "rn-chef")
                        ("/home/awatts/.rvm/gems/ruby-1.9.3-p551@chef-client/gems" "rn-chef gems")
                        ("/home/awatts/rubydev/workspace/mogilefs_s3_device" "mogilefs_s3_device")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@mogilefs_s3_device/gems" "mogilefs_s3_device gems")
                        ("/home/awatts/rubydev/workspace/god-local" "god-local")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@god-local/gems" "god-local gems")
                        ("/home/awatts/rubydev/workspace/nagios-config" "nagios-config")
                        ("/home/awatts/rpmbuild" "reverb-rpms")
                        ("/home/awatts/.rvm/src/ruby-1.9.3-p550" "ruby-1.9.3-p550")
                        ("/home/awatts/.rvm/rubies/ruby-1.9.3-p550/lib/ruby" "ruby-1.9.3-p550 (stdlib)")
                        ("/home/awatts/.rvm/src/ruby-1.9.3-p551" "ruby-1.9.3-p551")
                        ("/home/awatts/.rvm/rubies/ruby-1.9.3-p551/lib/ruby" "ruby-1.9.3-p551 (stdlib)")
                        ("/home/awatts/.rvm/src/ruby-2.0.0-p598" "ruby-2.0.0-p598")
                        ("/home/awatts/.rvm/rubies/ruby-2.0.0-p598/lib/ruby" "ruby-2.0.0-p598 (stdlib)")
                        ("/home/awatts/.rvm/src/ruby-2.1.5" "ruby-2.1.5")
                        ("/home/awatts/.rvm/rubies/ruby-2.1.5/lib/ruby" "ruby-2.1.5 (stdlib)")
                        ("/home/awatts/.rvm/rubies/rbx-2.2.10" "rbx-2.2.10")
                        ("/home/awatts/rubydev/workspace/MogileFS-Server" "MogileFS-Server")
                        ("/home/awatts/rubydev/workspace/MogileFS-Network" "MogileFS-Network")
                        ("/home/awatts/rubydev/workspace/MogileFS-Utils" "MogileFS-Utils")
                        ("/home/awatts/rubydev/workspace/perl-MogileFS-Client" "MogileFS-Client")
                        ("/home/awatts/Projects/mp3file" "mp3file")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@mp3file/gems" "mp3file gems")
                        ("/home/awatts/Projects/dreamybandnames" "dreamybandnames")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@dreamybandnames/gems" "dreamybandnames gems")
                        ("/home/awatts/Projects/songviz" "songviz")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@songviz/gems" "songviz gems")
                        ("/home/awatts/Projects/openhf" "openhf")
                        ("/home/awatts/.rvm/gems/ruby-2.1.5@openhf/gems" "openhf gems")
                        ("/home/awatts/Projects/euler" "euler")
                        ("/home/awatts/Projects/graphplay" "graphplay")
                        ("/home/awatts/Projects/graphplay-rs" "graphplay-rs")
                        ("/home/awatts/Projects/hub-pics" "hub-pics")
                        ("/home/awatts" "homedir")

                        ("/Users/awatts/rubydev/workspace/reverbnation" "reverbnation")
                        ("/Users/awatts/.rvm/gems/ruby-1.9.3-p550@reverbnation/gems" "reverbnation gems")
                        ("/Users/awatts/.rvm/gems/ruby-2.1.5@reverbnation/gems" "reverbnation gems (2.1)")
                        ("/Users/awatts/rubydev/workspace/redis_sendmail" "redis_sendmail")
                        ("/Users/awatts/.rvm/gems/ruby-2.0.0-p598@redis_sendmail/gems" "redis_sendmail gems")
                        ("/Users/awatts/rubydev/workspace/rn_image_server" "rn_image_server")
                        ("/Users/awatts/.rvm/gems/ruby-2.1.5@rn_image_server/gems" "rn_image_server gems")
                        ("/Users/awatts/rubydev/workspace/rn-chef" "rn-chef")
                        ("/Users/awatts/.rvm/gems/ruby-1.9.3-p551@chef-client/gems" "rn-chef gems")
                        ("/Users/awatts/rubydev/workspace/mogilefs_s3_device" "mogilefs_s3_device")
                        ("/Users/awatts/.rvm/gems/ruby-2.1.5@mogilefs_s3_device/gems" "mogilefs_s3_device gems")
                        ("/Users/awatts/rubydev/workspace/god-local" "god-local")
                        ("/Users/awatts/.rvm/gems/ruby-2.1.5@god-local/gems" "god-local gems")
                        ("/Users/awatts/rubydev/workspace/nagios-config" "nagios-config")
                        ("/Users/awatts/rpmbuild" "reverb-rpms")
                        ("/Users/awatts/.rvm/src/ruby-1.9.3-p550" "ruby-1.9.3-p550")
                        ("/Users/awatts/.rvm/rubies/ruby-1.9.3-p550/lib/ruby" "ruby-1.9.3-p550 (stdlib)")
                        ("/Users/awatts/.rvm/src/ruby-1.9.3-p551" "ruby-1.9.3-p551")
                        ("/Users/awatts/.rvm/rubies/ruby-1.9.3-p551/lib/ruby" "ruby-1.9.3-p551 (stdlib)")
                        ("/Users/awatts/.rvm/src/ruby-2.0.0-p598" "ruby-2.0.0-p598")
                        ("/Users/awatts/.rvm/rubies/ruby-2.0.0-p598/lib/ruby" "ruby-2.0.0-p598 (stdlib)")
                        ("/Users/awatts/.rvm/src/ruby-2.1.5" "ruby-2.1.5")
                        ("/Users/awatts/.rvm/rubies/ruby-2.1.5/lib/ruby" "ruby-2.1.5 (stdlib)")
                        ("/Users/awatts/.rvm/rubies/rbx-2.4.1" "rbx-2.4.1")
                        ("/Users/awatts/Projects/graphplay" "graphplay")
                        ("/Users/awatts/Projects/graphplay-rs" "graphplay-rs")
                        ("/Users/awatts/Projects/hub-pics" "hub-pics")
                        ("/Users/awatts" "homedir")

                        ("C:/Users/andrew/Projects/redis_sendmail" "redis_sendmail")
                        ("C:/Users/andrew/Projects/dreamybandnames" "dreamybandnames")
                        ("C:/Users/andrew/Projects/euler" "euler")
                        ("C:/Users/andrew/Projects/graphplay" "graphplay")
                        ("C:/Users/andrew/Projects/graphplay-rs" "graphplay-rs")
                        ("C:/Users/andrew/Projects/openhf" "openhf")
                        ("C:/Users/andrew/Projects/songviz" "songviz")
                        ("C:/Users/andrew/Projects/hub-pics" "hub-pics")
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
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'company-mode)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'ruby-end-mode)
  (add-hook 'ruby-mode-hook 'smartparens-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-ruby-mode)

;; Javascript / JSON configuration.
(defun ahw-configure-javascript ()
  (add-hook 'js2-mode-hook 'smartparens-mode)
  (add-hook 'json-mode-hook 'smartparens-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-javascript)

;; YAML configuration.
(defun ahw-turn-on-yaml-mode ()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-yaml-mode)

;; Clojure configuration
(defun ahw-configure-clojure-mode ()
  (require 'clojure-mode)
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
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-clojure-mode)

;; CIDER configuration
(defun ahw-configure-cider ()
  (require 'cider)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-cider)

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

(defun ahw-configure-es-mode ()
  (require 'es-mode)
  (require 'hideshow)
  (require 'hideshowvis)

  ;; Enable hide-show mode for es-mode.
  (add-to-list 'hs-special-modes-alist '(es-mode "{" "}" "/[*/]" nil))
  (add-to-list 'hs-special-modes-alist '(es-result-mode "{" "}" "/[*/]" nil))

  (add-hook 'es-mode-hook 'hideshowvis-symbols)
  (add-hook 'es-mode-hook 'hideshowvis-minor-mode)
  (add-hook 'es-mode-hook 'hs-minor-mode)
  (add-hook 'es-mode-hook 'smartparens-mode)

  (add-hook 'es-response-success-functions 'ahw-es-response-enable-hs-minor-mode)
  (add-hook 'es-response-success-functions 'ahw-es-response-reformat-json)

  (org-babel-do-load-languages 'org-babel-load-languages '((elasticsearch . t))))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-es-mode)

(defun ahw-configure-projectile ()
  (require 'projectile)
  (add-hook 'projectile-mode-hook 'projectile-rails-on))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-projectile)

(defun ahw-configure-robe ()
  (require 'robe)
  ;; (add-hook 'robe-mode-hook 'ac-robe-setup)
  )
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-robe)

(defun ahw-turn-off-auto-fill-mode ()
  (auto-fill-mode -1))

(defun ahw-configure-git-commit-mode ()
  (add-hook 'git-commit-mode-hook 'ahw-turn-off-auto-fill-mode)
  (add-hook 'git-commit-mode-hook 'visual-line-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-git-commit-mode)

(defun ahw-configure-rust-mode ()
  (add-hook 'rust-mode-hook 'smartparens-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-rust-mode)

(defun ahw-configure-dockerfile-mode ()
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("\\`Dockerfile" . dockerfile-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-dockerfile-mode)

(defun ahw-configure-c++-mode ()
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-c++-mode)

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

;; This works to maximize the window on OSX.
(when (eq window-system 'ns)
  (global-set-key (kbd "s-M") 'toggle-frame-maximized))

;; Load any .el files in user-lisp.
(dolist (f (directory-files "~/.emacs.d/user-lisp" t "\\.el\\'")) (load f))

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(c-offsets-alist
   (quote
    ((arglist-intro . +)
     (arglist-cont-nonempty . +)
     (arglist-close . 0)
     (template-args-cont . +))))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(debug-on-error nil)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(flx-ido-mode t)
 '(gdb-many-windows t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(json-reformat:indent-width 2)
 '(menu-bar-mode (window-system))
 '(mouse-yank-at-point t)
 '(nrepl-log-messages t)
 '(org-src-fontify-natively t)
 '(proced-auto-update-flag t)
 '(proced-filter (quote all))
 '(proced-tree-flag t)
 '(projectile-global-mode t)
 '(projectile-tags-command "regen_tags")
 '(rspec-key-command-prefix "s")
 '(rspec-use-opts-file-when-available nil)
 '(rspec-use-rvm t)
 '(ruby-deep-indent-paren (quote (t)))
 '(safe-local-variable-values
   (quote
    ((c-indent-offset . 4)
     (rust-indent-offset . 4)
     (c-basic-indent . 4)
     (encoding . binary)
     (encoding . utf-8)
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scss-compile-at-save nil)
 '(semantic-mode t)
 '(show-paren-mode t)
 '(smartparens-global-mode nil)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(sp-ignore-modes-list
   (quote
    (minibuffer-inactive-mode emacs-lisp-mode clojure-mode lisp-interaction-mode)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(visible-bell t)
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
