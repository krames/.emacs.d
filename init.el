;; == package mgmt ==========================================================


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(require 'uniquify)

(require 'rspec-mode)
(require 'yaml-mode)
(require 'coffee-mode)
(require 'chruby)
(require 'slim-mode)
(require 'web-mode)
(require 'ruby-tools)

;(add-hook 'ruby-mode-hook
 ;         (lambda () (rvm-activate-corresponding-ruby)))

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
(set-default-font "Monaco-16")
(set-face-attribute 'default t :font "Monaco-16")


;; -- coding -----------------------------------------------------------------
(setq tags-table-list
  '("./TAGS"))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
;(add-to-list 'auto-mode-alist '("\\.yml$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue" . vue-mode))

(setq enh-ruby-bounce-deep-indent t)
;;(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;;(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; == themes =================================================================

;; It's in Cask now instead /themes. Yay!
                                        ;(load-theme 'misterioso t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/blackboard-theme")
 (if window-system
     (load-theme 'blackboard t)
;;     (load-theme 'tsdh-light t)
	(load-theme 'wombat t))


;; == projectile setup from http://crypt.codemancers.com/posts/2013-09-26-setting-up-emacs-as-development-environment-on-osx/?utm_source=rubyweekly&utm_medium=email

;; == projectile

(require 'ivy)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;(global-set-key (kbd "C-x p") 'projectile-find-file)
;(global-set-key (kbd "C-x b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p k") 'projectile-kill-buffers)

;; ==  Hey, backspace, work right, dammit!
(normal-erase-is-backspace-mode 1)

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
(add-to-list 'ac-modes 'enh-ruby-mode)

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (rspec-mode 1)
            (make-local-variable 'ac-stop-words)
            (add-to-list 'ac-stop-words "end")
            (add-to-list 'ac-stop-words "do")))


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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "04c89cb30db1e9335a1f8e62307d3ba0e5ae5a007858497f376bc7a441953b3c" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "42ac06835f95bc0a734c21c61aeca4286ddd881793364b4e9bc2e7bb8b6cf848" "758da0cfc4ecb8447acb866fb3988f4a41cf2b8f9ca28de9b21d9a68ae61b181" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(helm-buffer-max-length 35)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (graphql-mode dockerfile-mode rainbow-delimiters go-autocomplete ## all-the-icons gotest vue-mode ac-ispell helm-spotify yaml-mode web-mode smex smart-mode-line slim-mode sass-mode ruby-tools ruby-end rspec-mode protobuf-mode projectile-rails multi-term markdown-mode magit helm-projectile grizzl go-projectile github-browse-file gist full-ack flx-ido enh-ruby-mode elixir-mode dash-at-point coffee-mode chruby cask ag)))
 '(ruby-deep-arglist (quote f)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin" ":" "/Users/krames/.go/bin/"))
(setenv "GOPATH" "/Users/krames/.go")
(setq exec-path (append exec-path (list "/usr/local/bin" "/Users/krames/.go/bin/")))

(add-to-list 'load-path "~/.emacs.d/vendor/")

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

(chruby "ruby-2.7.1")

  ;; define our own super awesome hook that will remove the before-save-hook
  (defun remove-enh-magic-comment ()
    (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))

  ;; add the hook to call our super awesome function.
  (add-hook 'enh-ruby-mode-hook 'remove-enh-magic-comment)

  ;; this is for ruby mode
  (setq ruby-insert-encoding-magic-comment nil)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-t") 'helm-projectile)
(global-set-key (kbd "s-g") 'projectile-ag)


(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "s-/") 'comment-dwim-line)


(setq display-time-default-load-average nil)
(display-time-mode)

(setq sml/mode-width 0)
(setq sml/name-width 20)
(rich-minority-mode 1)
(setf rm-blacklist "")
(sml/setup)

(add-to-list 'dash-at-point-mode-alist '(enh-ruby-mode . "ruby,rubygems,rails"))
(global-set-key (kbd "s-d") 'dash-at-point)
(put 'upcase-region 'disabled nil)


(setq ispell-program-name "/usr/local/bin/aspell")

(require 'gotest)
(define-key go-mode-map (kbd "C-c , v") 'go-test-current-file)
(define-key go-mode-map (kbd "C-c , s") 'go-test-current-test)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; (setq rspec-docker-container "web")
;; (setq rspec-use-docker-when-possible t)
;; (setq rspec-docker-cwd "/opt/spinweb/")

;; (setq rspec-docker-container "mds")
;; (setq rspec-use-docker-when-possible t)
;; (setq rspec-docker-cwd "/opt/mds/")
