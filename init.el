;; == package mgmt ==========================================================

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; -- load everything from dotfiles-init-dir ---------------------------------
(setq init-file (or load-file-name buffer-file-name))
(setq dotfiles-dir (file-name-directory init-file))
(setq dotfiles-init-dir (expand-file-name "init.d" dotfiles-dir))
(if (file-exists-p dotfiles-init-dir)
  (dolist (file (directory-files dotfiles-init-dir t "\\.el$"))
    (load file)))


;; == third-party packages ===================================================
(setq vendor-dir (expand-file-name "vendor" dotfiles-dir))
(dolist (project (directory-files vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'window-numbering)
(window-numbering-mode 1)
(show-paren-mode 1)

(require 'osx-clipboard)

(require 'sanity)

(require 'rspec-mode)
(require 'yaml-mode)
(require 'coffee-mode)


;; == window control =========================================================
;(add-to-list 'default-frame-alist '(height . 50))
;(add-to-list 'default-frame-alist '(width . 100))

(global-set-key "\C-c=" 'v-resize)
(global-set-key "\C-c}" 'h-resize)

; Successive -/= or {/} shrink/enlarge windows vertically/horizontally

(defun v-resize (key)
  "interactively resize the window vertically"
  (interactive "cHit -/= to enlarge/shrink")
  (cond
   ((eq key (string-to-char "="))
    (enlarge-window 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "-"))
    (enlarge-window -1)
    (call-interactively 'v-resize))
   (t (push key unread-command-events))))

(defun h-resize (key)
  "interactively resize the window horizontally"
  (interactive "cHit {/} to enlarge/shrink")
  (cond
   ((eq key (string-to-char "}"))
    (enlarge-window-horizontally 1)
    (call-interactively 'h-resize))
   ((eq key (string-to-char "{/}"))
    (shrink-window-horizontally 1)
    (call-interactively 'h-resize))
   (t (push key unread-command-events))))


;; -- M-g g is annoying
 (global-set-key (kbd "C-x l") 'goto-line)

;; -- disable stuff ----------------------------------------------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; -- rows and columns -------------------------------------------------------
(setq line-number-mode t)
(setq column-number-mode t)
(global-linum-mode t)

;; TODO turn off linum in multi-term dammit!

;; -- look -------------------------------------------------------------------
;(setq default-line-spacing 5)
(set-default-font "Monaco-14")

;; -- coding -----------------------------------------------------------------
(setq tags-table-list
  '("./TAGS"))

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("\\.yml$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq enh-ruby-bounce-deep-indent 1)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; == themes =================================================================

;; It's in Cask now instead /themes. Yay!
                                        ;(load-theme 'misterioso t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/blackboard-theme")
(load-theme 'blackboard t)
;;  (if window-system
;;      (load-theme 'blackboard t)
;; 	(load-theme 'wombat t))


;; == projectile setup from http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/?utm_source=rubyweekly&utm_medium=email

;; == projectile

(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(global-set-key (kbd "C-x p") 'projectile-find-file)
(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)

;; ==  Hey, backspace, work right, dammit!
(normal-erase-is-backspace-mode 0)

;; == browse on github
(global-set-key (kbd "C-c o") 'github-browse-file)

;; == gist

; gist private/public
(setq gist-view-gist t)
(global-set-key (kbd "C-c g r") 'gist-region-or-buffer-private)
(global-set-key (kbd "C-c g u") 'gist-region-or-buffer)

;; == auto-complete-mode
(auto-complete)
(auto-complete-mode)

; I want it everywhere!
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

; http://www.jefftk.com/p/emacs-auto-revert-mode
(global-auto-revert-mode 1)

;; == multi-term
(global-unset-key (kbd "C-x m"))
(define-key global-map (kbd "C-x m") 'multi-term)
(add-hook 'term-mode-hook 'linum-mode-off)

(defun linum-mode-off ()
  (interactive)
  (linum-mode 0))

;; == ruby

(add-hook 'ruby-mode-hook 'auto-complete-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "42ac06835f95bc0a734c21c61aeca4286ddd881793364b4e9bc2e7bb8b6cf848" "758da0cfc4ecb8447acb866fb3988f4a41cf2b8f9ca28de9b21d9a68ae61b181" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(ruby-deep-arglist (quote f)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin" ":" "/Users/krames/.go/bin"))
(setq exec-path (append exec-path '("/Users/krames/.go/bin")))
(setenv "GOPATH" "/Users/krames/.go")

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'go-autocomplete)
(require 'auto-complete-config)
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-x g") 'magit-status)
