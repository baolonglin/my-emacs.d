(setq gc-cons-threshold (* 128 1024 1024))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq p-org-roam-directory "~/RoamNotes")
(let ((private-file (expand-file-name ".private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file))
  )

;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

;; GUI
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

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(global-hl-line-mode +1)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
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
(use-package doom-themes  :demand t
  :init
  (load-theme 'doom-zenburn t)
  )
(use-package solarized-theme :demand t)

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

(use-package switch-window
  :custom
  (switch-window-shortcut-style 'alphabet)
  (switch-window-timeout nil)
  :bind ("C-x o" . switch-window)
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

(use-package selectrum
  :config
  (selectrum-mode +1)
  )

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package prescient)
(use-package selectrum-prescient)

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
(setq-default recentf-max-saved-items 1000)

;; Edit Utils
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq-default truncate-lines t)
(electric-pair-mode)
(electric-indent-mode)
(global-so-long-mode t)
(global-display-line-numbers-mode t)
(column-number-mode t)
(save-place-mode t)
(which-function-mode t)
(setq scroll-conservatively 100)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " â€¢ ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(global-prettify-symbols-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ibuffer-vc)
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
  :delight
  :config
  ;; autosave the undo-tree history
  ;;(setq undo-tree-history-directory-alist
  ;;      `((".*" . ,temporary-file-directory)))
  ;;(setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)
  )
(use-package goto-last-change)

(use-package plantuml-mode
  :mode "\\.uml\\'"
  :config
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (unless (file-exists-p plantuml-jar-path)
    (url-copy-file "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar" plantuml-jar-path))
  (setq plantuml-default-exec-mode 'jar)
  (setq org-plantuml-jar-path plantuml-jar-path)
  )
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

;; Git
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  )
(use-package git-timemachine)
(use-package magit
  :bind (("C-x g" . magit-status))
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

;; LSP
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-prefer-flymake nil)
  (setq lsp-file-watch-threshold 10000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-vetur-format-default-formatter-css "none")
  (setq lsp-vetur-format-default-formatter-html "none")
  (setq lsp-vetur-format-default-formatter-js "none")
  (setq lsp-vetur-validation-template nil)
  (setq lsp-lens-enable nil)
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
  (dap-auto-configure-mode)
  )
(use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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

;; Flycheck
(use-package flycheck
  :defer 2
  :delight
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  )

;; C++
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  )

;; JAVA
(use-package lsp-java
  :after (lsp-mode)
  :config
  (add-hook 'java-mode-hook 'lsp-deferred)
  (setq lsp-java-vmargs
        (list
              "-Xmx4G"
              "-Xms4G"
              "-Xss2m"
              "-XX:+UseG1GC"
              "-XX:+UseLargePages"
              )
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules" ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil

        ;; Configure lsp-java-format-settings-url to have format style file
        ;; Configure lsp-java-java-path to use different jdk
        lsp-enable-on-type-formatting t
        lsp-enable-indentation t)
  )

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

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))
	        (define-key special-event-map [sigusr1] 'signal-restart-server)
            ))

(provide 'init)
