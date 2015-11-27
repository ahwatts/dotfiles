;; -*- mode: emacs-lisp -*-

;; Start the server.
(server-mode)

;; PART 1 -- Package setup.

;; package.el configuration.
(require 'package)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://hiddencameras.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun ahw-package-install (package)
  "Custom package-install function that only installs a package if it has not
already been installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Create a hook that runs after installing packages that configures
;; packages that were potentially installed in that function.
(defvar ahw-after-installing-packages-hook nil
  "Hook called after ahw-install-packages runs.")

(defun ahw-install-packages ()
  "Function which installs various packages that we want to use."
  (unless package-archive-contents (package-refresh-contents))
  (dolist (p '(cider
               clojure-mode
               cmake-mode
               coffee-mode
               color-theme
               company
               dockerfile-mode
               ecb
               es-mode
               flx-ido
               flycheck
               flycheck-rust
               glsl-mode
               haml-mode
               hideshowvis
               ido-ubiquitous
               js2-mode
               json-reformat
               json-mode
               magit
               markdown-mode
               paredit
               paredit-menu
               projectile
               projectile-rails
               racer
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
               smex
               toml-mode
               unicode-fonts
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

;; PART 2 -- Keybindings changes.

;; ibuffer is nicer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Swap out the regular isearches for their regexp counterparts.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; ;; Org-mode shortcuts
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c b") 'org-iswitchb)

;; Something I do way too often.
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; ;; Random Ruby stuff
;; (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; Magit key bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; "Zoom".
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

;; PART 3 -- Package-specific configuration.

;; C++ configuration.
(defun ahw-configure-c++-mode ()
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-c++-mode)

;; CIDER configuration.
(defun ahw-configure-cider ()
  (require 'cider)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-cider)

;; Clojure configuration.
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

;; Company configuration
(defun ahw-configure-company ()
  (eval-after-load 'company '(push 'company-robe company-backends)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-company)

;; CMake configuration.
(defun ahw-turn-on-cmake-mode ()
  (require 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists" . cmake-mode)))
(add-hook 'after-installing-packages-hook 'ahw-turn-on-cmake-mode)

;; Dockerfile configuration.
(defun ahw-configure-dockerfile-mode ()
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("\\`Dockerfile" . dockerfile-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-dockerfile-mode)

;; Elisp configuration.
(defun ahw-configure-elisp-mode ()
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-elisp-mode)

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

;; Flycheck configuration.
(defun ahw-configure-flycheck ()
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-flycheck)

;; Git-commit configuration.
(defun ahw-turn-off-auto-fill-mode ()
  (auto-fill-mode -1))

(defun ahw-configure-git-commit-setup ()
  (add-hook 'git-commit-setup-hook 'ahw-turn-off-auto-fill-mode)
  (add-hook 'git-commit-setup-hook 'visual-line-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-git-commit-setup)

;; ielm configuration
(defun ahw-configure-ielm ()
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'company-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-ielm)

;; Javascript / JSON configuration.
(defun ahw-configure-javascript ()
  (add-hook 'js2-mode-hook 'smartparens-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'smartparens-mode)
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-javascript)

;; Magit configuration.
(defun ahw-configure-magit ()
  (setq magit-last-seen-setup-instructions "1.4.0"))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-magit)

;; Markdown configuration
(defun ahw-turn-on-markdown-mode ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-markdown-mode)

;; Paredit configuration -- add the paredit menu.
(defun ahw-configure-paredit-menu ()
  (require 'paredit-menu))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-paredit-menu)

;; Projectile configuration.
(defun ahw-configure-projectile ()
  (require 'projectile)
  (add-hook 'projectile-mode-hook 'projectile-rails-on))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-projectile)

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

;; Rust configuration.
(defun ahw-configure-rust-mode ()
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-rust-mode)

;; Smex configuration.
(defun ahw-configure-smex ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-smex)

;; Toml configuration.
(defun ahw-configure-toml-mode ()
  (add-to-list 'auto-mode-alist '("Cargo\\.lock\\'" . toml-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-configure-toml-mode)

;; YAML configuration.
(defun ahw-turn-on-yaml-mode ()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(add-hook 'ahw-after-installing-packages-hook 'ahw-turn-on-yaml-mode)

;; PART 4 -- Miscellaneous setup tasks.

;; Initialize colors for screen / tmux
(add-to-list 'load-path "~/.emacs.d/user-lisp")

;; This works to maximize the window on OSX.
(when (eq window-system 'ns)
  (global-set-key (kbd "s-M") 'toggle-frame-maximized))

;; Load any .el files in user-lisp.
(dolist (f (directory-files "~/.emacs.d/user-lisp" t "\\.el\\'")) (load f))

;; PART 5 -- Custom.

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
 '(company-tooltip-align-annotations t)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(debug-on-error nil)
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 '(flx-ido-mode t)
 '(gdb-many-windows t)
 '(git-commit-finish-query-functions nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(json-reformat:indent-width 2)
 '(mac-option-modifier (quote (:function alt :mouse alt :ordinary meta)))
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t)
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
    ((cider-boot-parameters . "dev")
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (js2-mode-show-strict-warnings)
     (c-indent-offset . 4)
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
