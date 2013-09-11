;; rvm-ruby-version-file-advice.el: Advise
;; rvm-activate-corresponding-ruby to look for .ruby-version and
;; .ruby-gemset in addition to .rvmrc

(defun ahw-ruby-version-file-locate (&optional path)
  (when (null path) (setq path default-directory))
  (cond
   ((equal (expand-file-name path) (expand-file-name "~")) nil)
   ((equal (expand-file-name path) "/") nil)
   ((member ".ruby-version" (directory-files path))
    (concat (expand-file-name path) "/.ruby-version"))
   (t (ahw-ruby-version-file-locate (concat (file-name-as-directory path) "..")))))

(defun ahw-ruby-gemset-file-locate (&optional path)
  (when (null path) (setq path default-directory))
  (cond
   ((equal (expand-file-name path) (expand-file-name "~")) nil)
   ((equal (expand-file-name path) "/") nil)
   ((member ".ruby-gemset" (directory-files path))
    (concat (expand-file-name path) "/.ruby-gemset"))
   (t (ahw-ruby-gemset-file-locate (concat (file-name-as-directory path) "..")))))

(defadvice rvm-activate-corresponding-ruby (around locate-ruby-version first (&optional path) activate)
  (let* ((ruby-version-file (ahw-ruby-version-file-locate path))
         (ruby-gemset-file (ahw-ruby-gemset-file-locate path))
         (ruby-version (when ruby-version-file
                         (with-temp-buffer
                           (insert-file-contents ruby-version-file)
                           (chomp (buffer-string)))))
         (ruby-gemset (when ruby-gemset-file
                        (with-temp-buffer
                          (insert-file-contents ruby-gemset-file)
                          (chomp (buffer-string))))))
    (message "ruby-version-file: %S ruby-gemset-file: %S ruby-version: %S ruby-gemset: %S"
             ruby-version-file ruby-gemset-file ruby-version ruby-gemset)
    (if (or ruby-version ruby-gemset)
        (rvm-use ruby-version ruby-gemset)
      ad-do-it)))
