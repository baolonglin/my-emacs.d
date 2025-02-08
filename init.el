;; -*- lexical-binding: t; -*-

(defun config/add-subdirs-to-load-path (parent-dir)
  (let* ((default-directory parent-dir))
    (setq load-path (append (seq-filter (lambda (dir) (file-directory-p dir))
                                        (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
                            load-path))
    )
  )

(setq gc-cons-threshold (* 128 1024 1024))

(setq p-org-roam-directory "~/RoamNotes")
(let ((private-file (expand-file-name ".private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file))
  )
(config/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; Font
(if (or (eq system-type 'windows-nt) (eq system-type 'gnu/linux))
    (cond
     ((find-font (font-spec :name "Source Code Pro"))
      (set-frame-font "Source Code Pro-12")
      (add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
      )
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (set-frame-font "DejaVu Sans Mono-12")
      (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
      )
     ((find-font (font-spec :name "inconsolata"))
      (set-frame-font "inconsolata-12")
      (add-to-list 'default-frame-alist '(font . "inconsolata-12"))
      )
     ((find-font (font-spec :name "Lucida Console"))
      (set-frame-font "Lucida Console-12")
      (add-to-list 'default-frame-alist '(font . "Lucida Console-12"))
      )
     ((find-font (font-spec :name "courier"))
      (set-frame-font "courier-12")
      (add-to-list 'default-frame-alist '(font . "courier-12"))
      ))
  )

(when (eq system-type 'darwin)
  (set-frame-font "Menlo 18")
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        )
  )

(use-package exec-path-from-shell
  :config
  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize)
    )
  )

(cond
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US")
  (setq ispell-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_GB") nil utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist)
  )

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(setq-default line-spacing 1)
(show-paren-mode 1)

;; Basic UI Cleanup
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode +1)
(global-hl-line-mode +1)
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

;; Key bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(windmove-default-keybindings)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Themes
(use-package modus-themes :demand t
  :init
  (load-theme 'modus-vivendi t)
  )

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; (doom-modeline-icon t)
  (doom-modeline-minor-modes t)
  ;;(doom-modeline-major-mode-icon t)
  ;;(doom-modeline-major-mode-color-icon t)
  (line-number-mode 1)
  (column-number-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package treemacs)

(use-package winner
  :config
  (winner-mode t))
(use-package popwin
  :config
  (popwin-mode t)
  (push '("*Warning*") popwin:special-display-config)
  )

;; Completion Framework
(use-package vertico
  :init
  (vertico-mode))

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind (
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

;; Grep
(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package rg
  :custom
  (rg-group-result t)
  (rg-show-columns t)
  )

;; Session
(use-package desktop
  :config
  (setq desktop-path (list user-emacs-directory)
	desktop-auto-save-timeout 600)
  (desktop-save-mode 1)
  (setq-default history-length 1000)
  )

(use-package session
  :config
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (session-initialize)
  )
(setq-default recentf-max-saved-items 1000)
(savehist-mode)

;; Edit Utils
(setq-default 
 indent-tabs-mode nil
 tab-width 4
 fill-column 100
 truncate-lines t
 electric-pair-mode t
 electric-indent-mode t
 column-number-mode t
 save-place-mode t
 which-function-mode t
 scroll-conservatively 100
 make-backup-files nil
 create-lockfiles nil
 auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t))
 uniquify-buffer-name-style 'reverse
 uniquify-separator " â€¢ "
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*"
)

(global-display-line-numbers-mode)
(setq global-prettify-symbols-mode t)
(setq global-so-long-mode t)

(use-package vlf)
(use-package beacon
  :delight
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (beacon-mode)
  )
(use-package delsel
  :defer t
  :init (delete-selection-mode))

(use-package undo-tree
  :pin "gnu"
  :diminish
  :bind (("C-x u" . undo-tree-visualize)
         ("M-_" . undo-tree-redo)
         ("M-n" . go-back-to-last-edit))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  ;; (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist '((".*" . "~/.emacs.d/var/autosaves/\\1")))
  ;; (setq undo-tree-visualizer-relative-timestamps nil)
  ;; (setq undo-tree-visualizer-timestamps t)
  :config
  (defun go-back-to-last-edit ()
    "Jump back to the last change in the current buffer."
    (interactive)
    (ignore-errors
      (let ((inhibit-message t))
        (undo-tree-undo)
        (undo-tree-redo))))
  :init
  (global-undo-tree-mode))

(use-package goto-last-change)

;; Languages
(use-package plantuml-mode
  :mode "\\.uml\\'"
  :config
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (unless (file-exists-p plantuml-jar-path)
    (url-copy-file "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar" plantuml-jar-path))
  (setq plantuml-default-exec-mode 'jar)
  (setq org-plantuml-jar-path plantuml-jar-path)
  )

;; Org
(use-package org
  :custom
  (org-confirm-babel-evalute nil)
  (org-image-actual-width nil)

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
     (sqlite . t)))
   (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

   (use-package org-bullets
     :hook (org-mode . org-bullets-mode)
     )
   )
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename p-org-roam-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-download
  :quelpa (org-download :fetcher github :repo "baolonglin/org-download")
  )

(use-package ox-mediawiki
  :quelpa (ox-mediawiki :fetcher github :repo "tomalexander/orgmode-mediawiki"))

(use-package org-remark
  :quelpa (org-remark :fetcher github :repo "nobiot/org-remark")
  :config
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode +1)
  )

;; Git
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  )

(use-package magit
  :bind (("C-x g" . magit-status))
  :init (if (not (boundp 'project-switch-commands))
            (setq project-switch-commands nil))
  :config
  (use-package magit-todos)
  )

(use-package git-gutter
  :delight
  :config
  (global-git-gutter-mode 't))

;; Programming
(use-package dumb-jump)

;; Whitespace
(setq-default show-trailing-whitespace t)
(use-package whitespace-cleanup-mode
  :delight
  :config
  (global-whitespace-cleanup-mode)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 300)
  )

;; Projectile
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

(use-package symbol-overlay
  :delight
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
	      ("M-i" . symbol-overlay-put)
	      ("M-n" . symbol-overlay-jump-next)
	      ("M-p" . symbol-overlay-jump-prev))
  )

;; Company
(use-package company
  :delight
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
    )

;; C++
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  )

;; Rust
(use-package rust-mode)

;; Go
(use-package go-mode)

;; Erlang
(use-package erlang)

;; Elisp
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))
(use-package paredit
  :delight
  :hook (emacs-lisp-mode . paredit-mode)
  :bind (:map paredit-mode-map
	     ("M-s" . nil))
  )
(use-package paredit-everywhere
  :delight
  :bind (:map paredit-everywhere-mode-map
		      ("M-s" . nil))
  :hook (prog-mode . paredit-everywhere-mode)
  )

(use-package origami
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
	      ("C-c f" . origami-recursively-toggle-node)
	      ("C-c F" . origami-toggle-all-nodes))
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package protobuf-mode)
(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package vue-mode
  :mode "\\.vue\\'"
  )

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package clojure-mode)
(use-package cider)
(use-package inf-clojure)

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
        python-shell-completion-native-enable nil)
  )
(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))


(use-package realgud)
(use-package realgud-lldb)

(use-package project-cmake
  :quelpa (project-cmake :fetcher github :repo "baolonglin/project-cmake")
  :config
  (require 'eglot)
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration)
  )

(use-package eglot
  :ensure t
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  )

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode)
  )

(defun config/chatgpt-el ()
  (let* ((chatgpt-bin (expand-file-name "site-lisp/chatgpt-el/chatgpt" user-emacs-directory)))
    (when (file-exists-p chatgpt-bin)
      (autoload 'chatgpt-query "chatgpt" nil t)
      (autoload 'chatgpt-insert-reply "chatgpt" nil t)
      (global-set-key "\C-cq" 'chatgpt-query)
      (global-set-key "\C-cQ" 'chatgpt-insert-reply)
      (setq chatgpt-prog chatgpt-bin)
      )
    )
  )

(config/chatgpt-el)


(use-package edit-server
  :config
  ;; chrome: https://chrome.google.com/extensions/detail/ljobjlafonikaiipfkggjbhkghgicgoh
  ;; firefox: https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
  (edit-server-start)
  )

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))
	        (define-key special-event-map [sigusr1] 'signal-restart-server)
            ))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
(put 'upcase-region 'disabled nil)

