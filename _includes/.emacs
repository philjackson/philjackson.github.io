(require 'package)

(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; minimally, install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(save-place-mode 1)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq ring-bell-function 'ignore
      visible-bell nil)

(add-hook 'after-init-hook 'server-start t)

(defun on-laptop? ()
  (string= (system-name) "phil-laptop"))

(set-face-attribute 'default nil
                    :font (concat "Source Code Pro "
                                  (if (on-laptop?)
                                      "14"
                                    "11")))

;; this makes using simple, inline lambdas much nicer
(defmacro interactively (&rest body)
  (declare (indent 0))
  `(lambda ()
     (interactive)
     ,@body))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil
              line-move-visual nil
              tab-width 2
              standard-indent 2)

;; emacs asks for the password, not a new popup
(setq epa-pinentry-mode 'loopback)
(setq show-trailing-whitespace t)

(setq browse-url-chrome-program "google-chrome-stable")
(setq browse-url-browser-function 'browse-url-firefox)

;; stop putting junk everywhere
(setq custom-file "~/.emacs.d/custom.el"
      backup-directory-alist `(("." . "~/.backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-default nil
      inhibit-startup-screen t)

;; load the custom file
(when (file-exists-p custom-file)
  (load custom-file))

(setq use-package-compute-statistics t)
(setq create-lockfiles nil)
(setq split-height-threshold 120
      split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers
vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

(use-package fira-code-mode
  :disabled
  :ensure t
  :custom (fira-code-mode-disabled-ligatures '("[]"))
  :hook prog-mode)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package bookmark
  :init
  (setq bookmark-default-file "~/bookmarks"
        bookmark-save-flag 1))

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 100)
  :config
  (recentf-mode t)
  (when (not (boundp 'recentf-timer))
    (message "New timer for recentf")
    (setq recentf-timer (run-at-time nil (* 5 60)
                                     (lambda ()
                                       (let ((save-silently t))
                                         (recentf-save-list)))))))

(use-package delight
  :ensure t)

(use-package evil
  :ensure t
  :delight undo-tree-mode
  :init (setq evil-default-cursor t
              evil-want-fine-undo t
              evil-want-integration t
              evil-shift-width 2
              evil-want-keybinding nil
              evil-insert-state-message nil
              evil-shift-round nil)
  :hook ((emacs-lisp-mode clojurescript-mode clojure-mode) . (lambda ()
                                                               (modify-syntax-entry ?_ "w")
                                                               (modify-syntax-entry ?- "w")))
  :config
  (evil-mode 1)
  (require 'goto-chg)
  ;; start a substitution on visual/object
  (evil-define-operator start-ex-sub-on-region (beg end)
    (let ((region (buffer-substring beg end)))
      (evil-ex (concat "%s/" (replace-regexp-in-string "\/" "\\\\/"
                                                       (regexp-quote region))
                       "/"))))
  (evil-global-set-key 'normal (kbd ",s") 'start-ex-sub-on-region)
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  package-menu-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil-leader
  :ensure t
  :after evil
  :config

  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "eb" 'eval-buffer
    "." 'xref-find-definitions)

  (evil-leader/set-key
    "!!" 'shell-command
    "!|" 'shell-command-on-region
    "ii" 'indent-region
    "ib" (interactively
           (indent-region (point-min) (point-max) nil))

    "sf" 'start-ex-sub-on-region

    "wv" 'split-window-right
    "wh" 'split-window-below
    "wb" 'balance-windows
    "wm" 'delete-other-windows
    "ww" 'other-window
    "wd" 'delete-window
    "w=" 'balance-windows

    "bd" 'kill-this-buffer

    "yb" (interactively (evil-yank (point-min) (point-max)))

    "u"  'undo-tree-visualize

    "nr" 'narrow-to-region
    "nf" 'narrow-to-defun
    "nw" 'widen

    "kt" 'transpose-sexps

    "dl" 'delete-matching-lines

    ;; as-in, clean
    "c" (interactively
          (delete-trailing-whitespace)
          (save-excursion
            (beginning-of-buffer)
            (replace-regexp "\n\n\n+" "\n\n"))))

  (global-evil-leader-mode t)

  (with-current-buffer "*Messages*"
    (evil-leader-mode t)))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :ensure t
  :init (setq js2-strict-missing-semi-warning nil
              js2-basic-offset 2))

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package git-timemachine
  :defer t
  :hook (git-timemachine-mode . evil-normalize-keymaps)
  :ensure t
  :after evil
  :config
  (evil-leader/set-key "gt" 'git-timemachine)
  (evil-make-overriding-map git-timemachine-mode-map 'normal))

(use-package edit-server
  :ensure t
  :init (setq edit-server-new-frame nil)
  :config (edit-server-start))

(use-package magit
  :ensure t
  :config
  ;; whole column for magit window, not a horizontal split as default
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (put 'magit-edit-line-commit 'disabled nil)
  (evil-leader/set-key
    "gf" 'magit-file-dispatch
    "gg" 'magit-dispatch
    "gb" 'magit-blame
    "gs" 'magit))

(use-package forge
  :after magit
  :ensure t
  :disabled
  :demand
  :bind (:map magit-status-mode-map
              ("." . #'forge-browse-dwim))
  :config
  (setq ghub-use-workaround-for-emacs-bug 'force)

  (setq magit-status-sections-hook (delete 'forge-insert-issues magit-status-sections-hook))
  (setq magit-status-sections-hook (delete 'forge-insert-pullreqs magit-status-sections-hook))

  (add-to-list 'magit-status-sections-hook 'forge-insert-authored-pullreqs t)
  (add-to-list 'magit-status-sections-hook 'forge-insert-assigned-issues t)
  ;;(add-to-list 'magit-status-sections-hook 'forge-insert-authored-issues t)
  (add-to-list 'magit-status-sections-hook 'forge-insert-requested-reviews t)

  (setq magit-section-initial-visibility-alist '((stashes . hide)
                                                 (issues . show)
                                                 (unpushed . show)
                                                 (pullreqs . show))))

(use-package undo-fu
  :ensure t
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package winum
  :ensure t
  :after (evil-leader)
  :init (setq winum-mode-line-position 1
              winum-auto-setup-mode-line nil
              winum-numbering-scope 'global)
  :config
  (winum-mode)
  (evil-leader/set-key
    "0" 'treemacs-select-window
    "1"  'winum-select-window-1
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5))

(use-package evil-commentary
  :ensure t
  :after evil
  :delight
  :disabled
  :config (evil-commentary-mode t))

(use-package moody
  :ensure t
  :disabled t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package doom-modeline
  :ensure t
  :init (setq doom-modeline-icon (display-graphic-p)
              doom-modeline-major-mode-icon t
              doom-modeline-lsp t
              doom-modeline-major-mode-color-icon t
              doom-modeline-height 25)
  :config (doom-modeline-init))

(use-package darktooth-theme
  :ensure t
  :disabled
  :config (load-theme 'darktooth t))

(use-package solarized-theme
  :ensure t
  :init (setq solarized-high-contrast-mode-line nil
              x-underline-at-descent-line t)
  :after org
  :disabled
  :config
  (load-theme 'solarized-light t)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil
                        :inherit 'normal))
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground "#cb4b16"))

(use-package rainbow-mode
  :delight
  :hook ((css-mode
          html-mode
          web-mode
          emacs-lisp-mode
          conf-mode) . rainbow-mode)
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/org")))

(use-package projectile
  :ensure t
  :delight
  :after evil-leader org
  :config
  (projectile-mode)
  (setq org-agenda-files (delete-dups (append org-agenda-files projectile-known-projects)))
  (evil-leader/set-key
    "pt" 'projectile-toggle-between-implementation-and-test))

(use-package counsel
  :ensure t
  :after evil-collection
  :delight ivy-mode
  :bind (("M-x" . #'counsel-M-x)
         :map counsel-find-file-map
         ("C-h" . #'counsel-up-directory)
         ("C-l" . #'ivy-alt-done))
  :init (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode t)
  ;; bigger ivy windows
  (setq ivy-height-alist '((t lambda (_caller) (/ (frame-height) 2))))
  ;; don't start commands with "^"
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    (kbd "C-SPC") 'ivy-restrict-to-matches
    (kbd "C-l") 'ivy-alt-done
    (kbd "C-j") 'ivy-next-line
    (kbd "C-k") 'ivy-previous-line)
  (evil-leader/set-key
    "P" #'counsel-yank-pop
    "bb" #'counsel-switch-buffer
    "ff" #'counsel-find-file
    "fp" #'counsel-git
    "fr" #'counsel-recentf))

(use-package wgrep
  :ensure t
  ;; :bind (:map ivy-occur-grep-mode-map
  ;;             ("e" . #'ivy-wgrep-change-to-wgrep-mode))
  :config
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal))

(defun search-specific-glob ()
  (interactive)
  (let ((glob (ivy-completing-read "Glob?: " '("*.cljs"
                                               "*.clj"
                                               "*.md"
                                               "*.styl"
                                               "*.css"))))
    (counsel-projectile-rg (concat "--glob " glob))))

(use-package counsel-projectile
  :ensure t
  :config
  ;; default action of `counsel-projectile-switch-project' will now be
  ;; to open dired.
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-dired)))

  (evil-leader/set-key
    "pc" 'counsel-projectile-org-capture
    "pa" 'counsel-projectile-org-agenda
    "pp" 'counsel-projectile-switch-project
    "ps" 'counsel-projectile-rg
    "pS" 'search-specific-glob))

(use-package flyspell
  :ensure t
  :delight
  :hook (((clojure-mode
           clojurescript-mode
           yaml-mode) . flyspell-prog-mode)
         ((text-mode) . flyspell-mode))
  :init (setq-default ispell-program-name "aspell"
                      ispell-local-dictionary "british")
  :config
  (evil-leader/set-key
    "sb" 'flyspell-buffer
    "sn" 'flyspell-goto-next-error)
  ;; don't spellcheck inline code faces for markdown-mode
  ;; (setq flyspell-generic-check-word-predicate
  ;;       #'(lambda ()
  ;;           (let ((f (get-text-property (- (point) 1) 'face)))
  ;;             (not (memq f '(markdown-pre-face
  ;;                            markdown-inline-code-face))))))
  )

(use-package clojure-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.snippets"))
  :delight yas-minor-mode
  :config (yas-global-mode 1))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config (global-evil-visualstar-mode))

(use-package json-mode
  :ensure t
  :init (setq js-indent-level standard-indent))

(use-package drag-stuff
  :ensure t
  :bind (:map evil-normal-state-map
              ("<up>"   . drag-stuff-up)
              ("<down>" . drag-stuff-down)
              :map evil-visual-state-map
              ("<up>"   . drag-stuff-up)
              ("<down>" . drag-stuff-down)))

(use-package evil-matchit
  :ensure t
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package expand-region
  :ensure t
  :config (define-key evil-normal-state-map (kbd ";") 'er/expand-region))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-text-zoom (:hint t)
    "Font size"
    ("k" text-scale-increase "up")
    ("j" text-scale-decrease "down")
    ("0" (text-scale-set 0) "reset"))
  (evil-leader/set-key
    "F"  'hydra-text-zoom/body))

(use-package evil-cleverparens
  :ensure t
  :hook ((clojure-mode
          emacs-lisp-mode
          clojurescript-mode
          inferior-clojure-mode) . evil-cleverparens-mode)
  :config
  (require 'evil-cleverparens-text-objects)
  (setq evil-move-beyond-eol t
        evil-cleverparens-use-regular-insert t
        evil-cleverparens-indent-afterwards nil
        evil-cleverparens-use-additional-bindings t))

(use-package smartparens
  :ensure t
  :config (require 'smartparens-config)
  :hook ((clojure-mode
          emacs-lisp-mode
          clojurescript-mode
          inferior-clojure-mode) . (lambda()
          (smartparens-mode)
          (smartparens-strict-mode))))

(use-package evil-mc
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-n" . evil-mc-make-and-goto-next-match)
              ("C-s" . evil-mc-skip-and-goto-next-match)
              ("C-;" . (lambda ()
                         (interactive)
                         (if (= 1 (evil-mc-get-cursor-count))
                             (evil-mc-make-all-cursors)
                           (evil-mc-undo-all-cursors))))
              :map evil-visual-state-map
              ("A" . evil-mc-make-cursor-in-visual-selection-end)
              ("I" . evil-mc-make-cursor-in-visual-selection-beg)
              ("C-n" . evil-mc-make-and-goto-next-match)
              ("C-;" . evil-mc-make-all-cursors))
  :demand
  :after smartparens
  :config
  (global-evil-mc-mode 1)
  (dolist (sp-command '(sp-up-sexp
                        sp-copy-sexp
                        sp-down-sexp
                        sp-join-sexp
                        sp-kill-sexp
                        sp-next-sexp
                        sp-split-sexp
                        sp-wrap-curly
                        sp-wrap-round
                        sp-raise-sexp
                        sp-clone-sexp
                        sp-wrap-square
                        sp-splice-sexp
                        sp-end-of-sexp
                        sp-forward-sexp
                        sp-backward-sexp
                        sp-convolute-sexp
                        sp-transpose-sexp
                        sp-kill-whole-line
                        sp-beginning-of-sexp
                        sp-forward-barf-sexp
                        sp-forward-slurp-sexp
                        sp-backward-barf-sexp
                        sp-backward-slurp-sexp
                        sp-splice-sexp-killing-forward
                        sp-splice-sexp-killing-backward))
    (add-to-list
     'evil-mc-custom-known-commands
     `(,sp-command
       (:default . evil-mc-execute-call)))))

(use-package which-key
  :ensure t
  :delight
  :config (which-key-mode t))

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :ensure t)

(use-package persistent-scratch
  :ensure t
  :init (setq persistent-scratch-save-file "~/.scratch.el")
  :config (persistent-scratch-setup-default))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode . emmet-mode)
  :bind (:map evil-insert-state-map
              ("M-j" . emmet-expand-line)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.handlebars" . web-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((clojurescript-mode
          clojure-mode
          emacs-lisp-mode) . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "dark orange" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "deep pink" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "chartreuse" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "deep sky blue" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "yellow" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "orchid" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "siennal" :weight 'bold))

(use-package highlight-parentheses
  :ensure t
  :disabled
  :config
  (setq highlight-parentheses-colors '("#fe7f2d"
                                       "#a1c181"
                                       "#fcca46"
                                       "#619b8a"
                                       "#233d4d"))
  :hook ((clojurescript-mode
          clojure-mode
          emacs-lisp-mode) . highlight-parentheses-mode))

(use-package company
  :ensure t
  :delight
  :init (setq company-idle-delay nil
              tab-always-indent 'complete
              company-dabbrev-downcase 0)
  :bind (:map evil-insert-state-map
              ("TAB" . 'company-indent-or-complete-common)
              :map evil-normal-state-map
              ("TAB" . 'company-indent-or-complete-common))
  :config (global-company-mode t))

(use-package aggressive-indent
  :ensure t
  :disabled
  :hook ((clojurescript-mode clojure-mode emacs-lisp-mode) . aggressive-indent-mode))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-fontify-code-blocks-natively t)
  :config
  (set-face-attribute 'markdown-header-face-1 nil
                      :height 150)
  (set-face-attribute 'markdown-header-face-2 nil
                      :height 130))

(use-package dired
  :bind ((:map dired-mode-map
               ("h" . dired-up-directory)
               ("g" . revert-buffer)
               ("e" . dired-toggle-read-only)))
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t
        dired-listing-switches "-alGhvF --group-directories-first")
  (evil-leader/set-key
    "fj" 'dired-jump))

;; extra font-lock in dired
(use-package diredfl
  :ensure t
  :hook ((dired-mode) . diredfl-mode))

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package evil-ediff
  :after ediff
  :ensure t)

(use-package evil-snipe
  :after magit
  :ensure t
  :init (setq evil-snipe-scope 'line)
  :config
  (evil-snipe-override-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package cider
  :ensure t
  :load-path "~/elisp"
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . 'cider-repl-clear-buffer))
  :after (evil evil-leader)
  :config (require 'cider-config))

(use-package lsp-mode
  :ensure t
  :hook (((clojure-mode clojurescript-mode sql-mode) . lsp))
  :init
  (setq lsp-lens-enable t
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-prefer-flymake nil)
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "rC" 'cider-jack-in'
    "rs" 'lsp-rename
    "rc" 'lsp-clojure-clean-ns
    "."  'lsp-find-definition)
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "rC" 'cider-connect-cljs
    "rs" 'lsp-rename
    "rc" 'lsp-clojure-clean-ns
    "."  'lsp-find-definition)
  (dolist (m '(clojurescript-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-enable nil)
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "," 'lsp-ui-peek-find-references)
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "," 'lsp-ui-peek-find-references)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode-hook . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point))

(use-package sql
  :ensure t
  :config
  (sql-set-product 'postgres)
  (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")
  (setq sql-connection-alist
        '((planner (sql-product 'postgres)
                   (sql-server "localhost")
                   (sql-user "phil")
                   (sql-password "phil")
                   (sql-database "planner-dev")))))

(use-package image-dired
  :ensure t
  :init (setq image-dired-thumb-size 400))

(use-package flycheck
  :ensure t
  :init (setq flycheck-emacs-lisp-load-path 'inherit)
  :config (setq-default flycheck-indication-mode 'left-margin))

(use-package tab-bar
  :init (setq tab-bar-close-button-show nil
              tab-bar-new-button-show nil)
  :disabled
  :config
  (tab-bar-mode 1)
  (evil-leader/set-key
    "tc" 'tab-bar-new-tab
    "tr" 'tab-bar-rename-tab
    "td" 'tab-bar-close-tab
    "tp" 'tab-bar-switch-to-prev-tab
    "tn" 'tab-bar-switch-to-next-tab))

(use-package stylus-mode
  :ensure t)

(use-package whitespace
  :init (setq whitespace-style '(face
                                 tabs
                                 trailing
                                 space-before-tab
                                 empty
                                 space-after-tab
                                 tab-mark
                                 missing-newline-at-eof))
  :hook ((clojure-mode clojurescript-mode emacs-lisp-mode) . whitespace-mode)
  :delight
  :config (set-face-attribute 'whitespace-space nil :foreground "#444"))

(use-package doom-themes
  :ensure t
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
  :config
  ;; list of themes (just called as functions) which also contain
  ;; theme specific config
  ;; (let* ((funs '((lambda () (load-theme 'doom-outrun-electric))
  ;;                (lambda () (load-theme 'doom-palenight))
  ;;                (lambda () (load-theme 'doom-wilmersdorf))
  ;;                (lambda () (load-theme 'doom-snazzy))
  ;;                (lambda ()
  ;;                  (load-theme 'doom-challenger-deep t)
  ;;                  (set-face-attribute 'markdown-code-face nil
  ;;                                      :background "#32333d")
  ;;                  (set-face-attribute 'font-lock-comment-face nil
  ;;                                      :foreground "#999"))
  ;;                (lambda ()
  ;;                  (load-theme 'doom-dracula t)
  ;;                  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
  ;;                                      :foreground "pink"))))
  ;;        (rand (random (length funs))))
  ;;   (funcall (nth rand funs)))

  (load-theme 'doom-challenger-deep t)

  ;; any global theme config goes here
  (set-face-attribute 'show-paren-match nil :foreground "#111" :background "orange")
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t)

(use-package swiper
  :ensure t
  :after ivy)

(use-package cljr-ivy
  :ensure t
  :disabled
  :after ivy
  :config
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "rr" 'cljr-ivy)
  (evil-leader/set-key-for-mode 'clojure-mode
    "rr" 'cljr-ivy))

(use-package flyspell-correct-ivy
  :ensure t
  :after ivy
  :config
  (evil-leader/set-key
    "sw" 'flyspell-correct-wrapper))

(use-package ivy-xref
  :ensure t
  :after (ivy)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs
        xref-show-definitions-function #'ivy-xref-show-defs))

(use-package ivy-rich
  :ensure t
  :after counsel ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package inf-clojure
  :ensure t)

(use-package diff-hl
  :ensure t
  :disabled
  :demand
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)
         (dired-mode-hook . diff-hl-dired-mode))
  :config (global-diff-hl-mode t))

(use-package crux
  :ensure t
  :config
  (evil-leader/set-key
    "id" 'crux-indent-defun
    "fe" 'crux-find-user-init-file))

(use-package literate-calc-mode
  :ensure t
  :config
  (setq initial-major-mode 'literate-calc-mode))

(use-package calc-units
  :config
  ;; unit conversion
  (defalias 'calcFunc-uconv 'math-convert-units)
  (setq math-additional-units
        '((eur "1.22 * usd" "Euro")
          (gbp "1.34 * usd" "British Pound")
          (usd nil "United States Dollar"))
        math-units-table nil))

(use-package treemacs
  :ensure t
  :disabled
  :config
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (treemacs-follow-mode)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil
  :ensure t
  :disabled
  :after treemacs evil)

(use-package treemacs-magit
  :after treemacs magit
  :disabled
  :ensure t)

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package counsel-lsp-clj
  :quelpa (counsel-lsp-clj
           :fetcher github
           :repo "philjackson/counsel-lsp-clj")
  :config
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "rr" 'counsel-lsp-clj-refactorings)
  (evil-leader/set-key-for-mode 'clojure-mode
    "rr" 'counsel-lsp-clj-refactorings))

;; edit input fields with emacs
(use-package emacs-everywhere
  :quelpa (emacs-everywhere
           :fetcher github
           :repo "tecosaur/emacs-everywhere"))

(use-package dap-mode
  :ensure t
  :disabled
  :config (require 'dap-chrome))

(use-package mermaid-mode
  :mode "\\.mermaid\\'"
  :ensure t)

;; copy the github url for current file/line
(use-package browse-at-remote
  :ensure t
  :config
  (evil-leader/set-key
    "gy" 'browse-at-remote-kill))

(use-package tramp
  :config
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

(defun yadm ()
  (interactive)
  (magit-status "/yadm::"))

;; alignmet - https://github.com/edkolev/evil-lion
(use-package evil-lion
  :ensure t
  :after evil
  :config (evil-lion-mode t))

(use-package posframe
  :ensure t)

(use-package evil-owl
  :ensure t
  :after (posframe evil)
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-idle-delay 0.5
        evil-owl-max-string-length 50)
  (evil-owl-mode t))

(use-package desktop
  :disabled
  :init (setq desktop-dirname             "~/.emacs.d/"
              desktop-path                (list desktop-dirname)
              desktop-base-file-name      "desktop.el"
              desktop-base-lock-name      "lock"
              desktop-restore-eager       10
              desktop-save                t
              desktop-files-not-to-save   "^$"
              desktop-load-locked-desktop nil
              desktop-auto-save-timeout   30)
  :config (desktop-save-mode 1))

(defun my-desktop-save ()
  (interactive)
  ;; don't call desktop-save-in-desktop-dir as it prints a message
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'my-desktop-save)

(use-package evil-fringe-mark
  :ensure t
  :init (setq-default left-margin-width 2)
  :config (global-evil-fringe-mark-mode))

(use-package vimish-fold
  :ensure t)

;; zf - create fold
;; zd - delete fold
(use-package evil-vimish-fold
  :ensure t
  :after (vimish-fold evil)
  :config (global-evil-vimish-fold-mode 1))

(use-package restclient
  :ensure t
  :mode (("\\.restclient\\'" . restclient-mode)))

(use-package evil-collection
  :ensure t
  :after magit evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))
