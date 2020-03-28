;; Make all commands of the “package” module present.
(require 'package)

;; Speef up start up by not loading any packages at startup.
;; (setq package-enable-at-startup nil)
;; Look at the *Messages* buffer before setting this to nil, then after.

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Adjust garbage collection thresholds
(setq gc-cons-threshold (* 128 1024 1024))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun config/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (seq-filter
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))
(config/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; load private configuration data
(let ((private-file (expand-file-name ".private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file))
  )

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; Maintainer is AWOL.
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

;; Actually get “package” to work.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package)

;; GUI
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(setq-default line-spacing 1)
(show-paren-mode 1)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(set-fringe-mode '(10 . 0))
(when (boundp 'fringe-indicator-alist)
  (setq-default fringe-indicator-alist
		'(
		  (continuation . nil)
		  (overlay-arrow . nil)
		  (up . nil)
		  (down . nil)
		  (top . nil)
		  (bottom . nil)
		  (top-bottom . nil)
		  (empty-line . nil)
		  (unknown . nil))))

(use-package diminish)
(use-package command-log-mode)

;; Grep
(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :config
  (use-package wgrep-ag)
  )

(use-package rg
  :custom
  (rg-group-result t)
  (rg-show-columns t)
  )

;; Uniquify
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package ibuffer-vc)

;; Flycheck
(use-package flycheck
  :defer 2
  :diminish
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  )

;; Recentf
(setq-default recentf-max-saved-items 1000)

;; Window
(use-package switch-window
  :custom
  (switch-window-shortcut-style 'alphabet)
  (switch-window-timeout nil)
  :bind ("C-x o" . switch-window)
  )

;; Session
(use-package desktop
  :config
  (setq desktop-path (list user-emacs-directory)
	desktop-auto-save-timeout 600)
  (desktop-save-mode 1)
  (setq-default history-length 1000)
  (setq desktop-globals-to-save
	'((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (ido-buffer-history       . 100)
          (ido-last-directory-list  . 100)
          (ido-work-directory-list  . 100)
          (ido-work-file-list       . 100)
          (ivy-history              . 100)
          (magit-read-rev-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))
  )

(use-package session
  :config
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (session-initialize)
  )

;; Edit Utils
(global-display-line-numbers-mode t)
(column-number-mode t)
(save-place-mode t)
(which-function-mode t)
(setq scroll-conservatively 100)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-pair-mode)
(electric-indent-mode)
(setq-default truncate-lines nil)
(use-package vlf)
(use-package beacon
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (beacon-mode)
  )

(global-prettify-symbols-mode)
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
	      ("M-i" . symbol-overlay-put)
	      ("M-n" . symbol-overlay-jump-next)
	      ("M-p" . symbol-overlay-jump-prev))
  )

(use-package avy
  :bind ("M-s" . avy-goto-char))
(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode)
  )

(use-package which-key
 :diminish which-key-mode
 :init (which-key-mode)
 :config (which-key-setup-side-window-right)
         (setq which-key-idle-delay 0.05)
	 )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Compile
(use-package compile
  :config
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer)

  (setq compilation-scroll-output t)
  )

;; Whitespace
(setq-default show-trailing-whitespace t)
(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 300)
  )

;; Version control
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  )
(use-package git-timemachine)
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (use-package magit-todos)
  (use-package magit-popup)
  (use-package magit-gerrit
    :load-path ("site-lisp/magit-gerrit")
    )
  )

;; Regex
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\.html\\'"
  )

;; Plantuml
(use-package plantuml-mode
  :mode "\\.uml\\'"
  :custom
  (plantuml-jar-path "~/.emacs.d/plantuml.jar")
  :config
  (unless (file-exists-p plantuml-jar-path)
    (url-copy-file "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar" plantuml-jar-path))
  )

;; Org
(use-package org
  :custom
  (org-plantuml-jar-path plantuml-jar-path)
  (org-confirm-babel-evalute nil)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (C . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t)
     (mscgen . t)))

  (use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  )
  (diminish 'org-indent-mode)
  )

(diminish 'eldoc-mode)
(diminish 'subword-mode)

(use-package restart-emacs
  :commands restart-emacs)

(use-package doom-themes  :demand t
  :init
  (load-theme 'doom-dark+ t)
  )
(use-package solarized-theme :demand t)

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-line-column-p t)
  ;;(setq spaceline-line-p nil)
  (setq powerline-default-separator 'arrow)
  (spaceline-toggle-which-function-on)
  :init
  (spaceline-spacemacs-theme)
 )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package company
  :diminish
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :config
    (setq company-dabbrev-other-buffers t
          company-dabbrev-code-other-buffers t
          company-complete-number t
          company-show-numbers t
          company-minimum-prefix-length 2
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case t
          company-idle-delay 0
          )
    (global-company-mode 1)

    (use-package company-anaconda
      :ensure t
      :config
      (add-to-list 'company-backends 'company-anaconda)
      (use-package rx)
      )
    )

(use-package historian)
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")
  (setq enable-recursive-minibuffers t)
  (setq projectile-completion-system 'ivy)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	;; '((t . ivy--regex-plus))
	;; allow input not in order
        '((t . ivy--regex-ignore-order))
	;; '((t . ivy--regex-fuzzy))
	)
  (use-package ivy-historian
    :config
    (ivy-historian-mode 1)
    )
  )

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :config
  (setq-default consel-mode-override-describe-bindings t)
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")
    )
  )

(use-package undo-tree
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)
  )

(use-package rfc-mode
  :config
  (setq rfc-mode-directory (expand-file-name "~/rfc/")))

(use-package google-translate
  :defer t
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "zh-CN")
  )

;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)
	 )
  )

;; Yaml
(use-package yaml-mode
  :mode "\\.yml\\'"
  )

;; Paredit
(use-package paredit
  :bind (:map paredit-mode-map
	     ("M-s" . nil))
  )
(use-package paredit-everywhere
    :bind (:map paredit-everywhere-mode-map
		("M-s" . nil))
    :hook (prog-mode . paredit-everywhere-mode)
    )

;; Folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
	      ("C-c f" . origami-recursively-toggle-node)
	      ("C-c F" . origami-toggle-all-nodes))
  )
;; Misc
(setq-default fill-column 100)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(global-hl-line-mode +1)
(defun signal-restart-server ()
  "Handler for SIGUSR1 signal, to (re)start an Emacs server.
Can be tested from within Emacs with:
  (signal-process (emacs-pid) 'sigusr1)

or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start))
(define-key special-event-map [sigusr1] 'signal-restart-server)


(use-package elfeed)

(use-package treemacs)
(use-package lsp-mode
  :hook ((java-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-prefer-capf t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-prefer-flymake nil)
  (setq lsp-file-watch-threshold 10000)
  (setq read-process-output-max (* 1024 1024))

  (use-package company-lsp
    :after (lsp-mode)
    :custom
    (company-lsp-cache-candidates t)
    (company-lsp-async t)
    :config (push 'company-lsp company-backends))
  (use-package lsp-java
    :after (lsp-mode)
    :config
    (setq lsp-java-vmargs
      (list "-noverify"
            "-Xmx2G"
            "-XX:+UseG1GC"
            "-XX:+UseStringDeduplication"
            )
      lsp-file-watch-ignored
      '(".idea" ".ensime_cache" ".eunit" "node_modules" ".git" ".hg" ".fslckout" "_FOSSIL_"
        ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build")

      lsp-java-import-order '["" "java" "javax" "#"]
      ;; Don't organize imports on save
      lsp-java-save-action-organize-imports nil

      ;; Formatter profile
      ;; lsp-java-format-settings-url (concat "file://" p-java-format-file)
      lsp-enable-on-type-formatting t
      lsp-enable-indentation t)
    )
  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t)
    )
  (use-package dap-mode
    :after (lsp-mode)
    :config
    (dap-mode t)
    (dap-ui-mode t)
    (require 'dap-java)
    (dap-register-debug-template
     "localhost:8888"
     (list :type "java"
           :request "attach"
           :hostName "localhost"
           :port 8888))
    )
  (use-package lsp-ivy)
  (use-package lsp-treemacs)
  )

;; C++
(use-package clang-format
  :functions clang-format-buffer
  :init
  (defun clang-format-defun ()
    (interactive)
    (save-excursion
      (mark-defun)
      (clang-format-region (region-beginning) (region-end))
      (deactivate-mark)))
  :config
  (setq-default clang-format-fallback-style "webkit")
  )

(use-package rtags
  :if (executable-find "rdm")
  :bind (:map c-mode-map
	      ("M-." . rtags-find-symbol-at-point)
	      ("C-." . rtags-find-references-at-point)
	      ("M-," . rtags-location-stack-back)
	      ("C-," . rtags-location-stack-forward)
	 :map c++-mode-map
	      ("M-." . rtags-find-symbol-at-point)
	      ("C-." . rtags-find-references-at-point)
	      ("M-," . rtags-location-stack-back)
	      ("C-," . rtags-location-stack-forward)
	      )
  :hook ((c++-mode . rtags-start-process-unless-running)
	 (c-mode . rtags-start-process-unless-running))
  :config
  (rtags-enable-standard-keybindings c++-mode-map)
  (rtags-enable-standard-keybindings c-mode-map)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)

  (use-package flycheck-rtags
    :functions flycheck-select-checker setup-flycheck-rtags
    :config
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      ;; RTags creates more accurate overlays.
      ;; (setq-local flycheck-highlighting-mode nil)
      ;; (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    )
  (use-package company-rtags
    :init
    (eval-after-load 'company
      '(push 'company-rtags company-backends))
    :bind (:map c-mode-base-map
		("C-<tab>" . company-complete))
    :config
    (setq company-global-modes '(not gud-mode))
    )
  (use-package ivy-rtags
    :init
    (setq rtags-display-result-backend 'ivy)
    )
  )

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode lsp)
  )

(use-package fullframe
  :config
  (fullframe magit-statu4s magit-mode-quit-window nil)
  (fullframe ibuffer ibuffer-quit nil)
  )

(use-package web-mode
  :mode "\\.erb\\.tpl\\.php\\.html?\\'"
  )

(use-package projectile
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Proj"
       (format " Proj[%s]" (projectile-project-name))))
   projectile-enable-caching t)
  (projectile-mode +1)
  )

(use-package ag
  :ensure-system-package ag
  :commands (ag ag-regexp ag-project))

(use-package rg
  :ensure-system-package rg
  :commands (rg))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  )

(use-package xclip
  :if (eq system-type 'gnu/linux)
  :config
  (xclip-mode t)
  )

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  )

(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load-file local-file))
  )

;; emacsclient access
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(use-package atomic-chrome
  ;; dependency Atomic Chrome extension (in Chrome)
  :init
  ;;(setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-extension-type-list '(atomic-chrome))
  (defun ac/compile-current-buffer ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (when (not file-name)
        (setq file-name "/tmp/ac-compile-current-buffer.cc")
        )
      (write-file file-name)
      (compile (format "g++ -Wall %s" file-name))
      (when (executable-find "rc")
        (shell-command (format "rc -c g++ -Wall -c %s" file-name))
        )
      )
    )
  :config
  (atomic-chrome-start-server))

(when (file-exists-p custom-file)
  (load custom-file))
