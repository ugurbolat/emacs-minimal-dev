;;; main.el --- Emacs Config for Minimal Development Environment -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ugur Bolat
;;
;; Author: Ugur Bolat <dev@bolat.xyz>
;; Maintainer: Ugur Bolat <dev@bolat.xyz>
;; Created:  February 13, 2024

;; Package-Requires: ((emacs "30.0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs configuration file that aims minimal development environment.
;; Currently, python is the main development language.

;; elpaca
;; place elpaca packages into different location than default
;; wrt emacs version
(setq ub/emacs-configs-dir "~/emacs-configs")
(defvar elpaca-directory (expand-file-name (concat "elpaca-emacs-vanilla-really-not-really_" emacs-version) ub/emacs-configs-dir))
;; check if ub/elpaca-dir exits, if not create it
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))

(defvar elpaca-installer-version 0.7)
;;(defvar elpaca-directory (expand-file-name "elpaca/" ub/elpaca-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; elpaca

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;; traditional strip-down
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; good defaults
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t) ; open scratch buffer at startup
;; after emacs startup screen, open scratch buffer (instead of using inhibit-startup-screen, use hook)
;;(add-hook 'emacs-startup-hook 'switch-to-buffer)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; close the startup screen GNU Emacs buffer and window
            ;;(delete-window (get-buffer-window "*GNU Emacs*"))
            (delete-window)
            (switch-to-buffer "*scratch*")
            (lisp-interaction-mode)))
(setq initial-scratch-message "")
(setq indent-tabs-mode nil) ; stop using tabs to indent
(setq make-backup-files nil) ; stop creating ~ backup files
(require 'autorevert)
(global-auto-revert-mode 1) ; load recent changes done outside
(setq auto-revert-use-notify t) ; and notify
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq enable-local-variables 't) ; local variables are customizations in either file or directory
(setq scroll-step 1
      scroll-conservatively 10000) ;; better scroll: go one line up or down, not half the screen.
(global-hl-line-mode 1) ;; horizontal current line highlight
(setq cursor-type 'box)

;;;; FROM DOOM
;;
;;; Formatting
;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)
;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)
;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
;;(setq tabify-regexp "^\t* [ \t]+")
;; An archaic default in the age of widescreen 4k displays? I disagree. We still
;; frequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)
;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)
;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)
;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)
;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)


;; undo/redo madness
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; resizing windows,
;; better keybindings than default C-x {,} and C-x ^,= which are both hard to reach and multi-key combinations
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)



;; theme
(elpaca
 (doom-themes
  :fetcher github
  :repo "doomemacs/themes"))
(elpaca-wait)
(load-theme 'modus-vivendi t)
;(load-theme 'doom-pine t)
(set-face-attribute 'default (selected-frame) :height 160)
;; setting same face w/ eglot highlight
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))
(setq fringe-mode 'default)
(set-fringe-style (quote (12 . 8)))
;; modeline

(elpaca
    (doom-modeline
     :fetcher github
     :repo "seagle0128/doom-modeline"))
(elpaca-wait)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  ;; :config
  ;; (setq doom-modeline-minor-modes t)
  ;; (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil))
    
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


;; A minor-mode menu for the mode line 
;; tarsius/minions
(elpaca
 (minions
  :fetcher github
  :repo "tarsius/minions"))
(elpaca-wait)
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(elpaca
 (hl-todo
  :fetcher github
  :repo "tarsius/hl-todo"))
(elpaca-wait)
(use-package hl-todo
    :config
    (global-hl-todo-mode 1)
    (setq hl-todo-keyword-faces
            '(("TODO" . "#ff4500")
            ("DONT" . "#70b900")
            ("NEXT" . "#b6a0ff")
            ("BUG" . "#C70039")
            ("DONE" . "#44bc44")
            ("NOTE" . "#d3b55f")
            ("HOLD" . "#c0c530")
            ("HACK" . "#d0bc00")
            ("FAIL" . "#ff8059")
            ("WORKAROUND" . "#ffcccc")
            ("FIXME" . "#ff9077")
            ("REVIEW" . "#6ae4b9")
            ("DEPRECATED" . "#bfd9ff")
            ("REF" . "#660066"))))
 
;; (global-hl-todo-mode 1)
;; (setq hl-todo-keyword-faces
;;       '(("TODO" . "#ff4500")
;;         ("DONT" . "#70b900")
;;         ("NEXT" . "#b6a0ff")
;;         ("BUG" . "#C70039")
;;         ("DONE" . "#44bc44")
;;         ("NOTE" . "#d3b55f")
;;         ("HOLD" . "#c0c530")
;;         ("HACK" . "#d0bc00")
;;         ("FAIL" . "#ff8059")
;;         ("WORKAROUND" . "#ffcccc")
;;         ("FIXME" . "#ff9077")
;;         ("REVIEW" . "#6ae4b9")
;;         ("DEPRECATED" . "#bfd9ff")
;;         ("REF" . "#660066")))


(recentf-mode 1)

;; eglot+tramp remote setup
;; TODO document the eglot worflow when in tramp
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; helpful
(elpaca
 (which-key
  :fetcher github
  :repo "justbur/emacs-which-key"))

;; which-key
(elpaca
 (helpful
  :fetcher github
  :repo "Wilfred/helpful"))
(elpaca-wait)
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; emacs rotate window layout
(elpaca
 (rotate
  :fetcher github
  :repo "daichirata/emacs-rotate"))

;; zoom in/out/increase font
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; ace-window
(elpaca
 (ace-window
  :fetcher github
  :repo "abo-abo/ace-window"))
(elpaca-wait)
(global-set-key (kbd "C-x o") 'ace-window)

;; dired
(elpaca
 (dired-hacks
  :fetcher github
  :repo "Fuco1/dired-hacks"))
;; Block until current queue processed.
(elpaca-wait)
;; dired
(use-package dired
  :config
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;; TODO continue to wrap w/ use-package
;; dired extra
(require 'dired-x)
(when (eq system-type 'gnu/linux)
  (setq dired-guess-shell-alist-user '(("\\.mp3\\'"  "mpv")
                                       ("\\.mp4\\'"  "mpv")
                                       ("\\.m4a\\'"  "mpv")
                                       ("\\.webm\\'" "mpv")
                                       ("\\.mkv\\'"  "mpv")
                                       ("\\.avi\\'"  "mpv")
                                       ("\\.pdf\\'" "okular")
                                       ("\\.pd\\'"  "okular")
                                       ("\\.dvi\\'" "okular")
                                       ("\\.epub\\'" "ebook-viewer")
                                       ("\\.doc\\'" "libreoffice")
                                       ("\\.docx\\'" "libreoffice")
                                       ("\\.ppt\\'" "libreoffice")
                                       ("\\.pptx\\'" "libreoffice")
                                       ("\\.xls\\'" "libreoffice")
                                       ("\\.xlsx\\'" "libreoffice")
                                       ("\\.odt\\'" "libreoffice")
                                       ("\\.ods\\'" "libreoffice")
                                       ("\\.odg\\'" "libreoffice")
                                       ("\\.odp\\'" "libreoffice")
                                       ("\\.jpg\\'" "eog")
                                       ("\\.jpeg\\'" "eog")
                                       ("\\.png\\'" "eog")
                                       ("\\.gif\\'" "eog")
                                       ("\\.svg\\'" "eog")
                                       )))
;; dired subtree
(require 'dired-subtree)
(setq dired-subtree-use-backgrounds nil)

;; dired ediff marked files
(defun ub/ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

;; dired keybidings
(global-set-key (kbd "C-c o -") 'dired-jump)
(let ((map dired-mode-map))
  (define-key map (kbd "<tab>") #'dired-subtree-toggle)
  (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
  (define-key map (kbd "<backtab>") #'dired-subtree-remove))
(define-key dired-mode-map "e" 'ub/ediff-files)
;; going in and out into folder with easier keys
;; M-right arrow going into the directory at point
;; M-left going out back to the up directory
(define-key dired-mode-map (kbd "<M-right>") 'dired-find-file)
(define-key dired-mode-map (kbd "<M-left>") 'dired-up-directory)


;; consult, vertico
(elpaca
 (consult
  ;; Inherited from elpaca-menu-item.
  ;;:files (:defaults)
  :fetcher github
  :repo "minad/consult"))
(elpaca
 (vertico
  :fetcher github
  :repo "minad/vertico"))
(elpaca
 (orderless
  :fetcher github
  :repo "oantolin/orderless"))
(elpaca
 (marginalia
  :fetcher github
  :repo "minad/marginalia"))


;; Block until current queue processed.
(elpaca-wait)

;; Enable vertico
;; REF: https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package marginalia
  :config
  (marginalia-mode))


;; TODO achieve similar state to Doom's consult+vectico+orderless
;; e.g.,
;; when backspace is pressed for deleting a path in minibuffer, deletes the word
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
;; when typing path in minibuffer, it is fully fuzy!


;; keybindings for consult
(global-set-key (kbd "C-c f r") 'consult-recent-file)
(global-set-key (kbd "C-x C-v") 'consult-buffer)
(global-set-key (kbd "C-x C-'") 'consult-grep)
(global-set-key (kbd "C-s") 'consult-line)

(setq consult-line-start-from-top 't)


;; functions that use `consult-find` w/ default dirs
;; signature (defun consult-find (&optional dir initial)
(defun ub/consult-find-main (&optional initial)
  "Search for files with `find' in ~/main."
  (interactive "P")
  (consult-find "~/main" initial))
(defun ub/consult-find-home (&optional initial)
  "Search for files with `find' in ~/"
  (interactive "P")
  (consult-find "~/" initial))
(defun ub/consult-find-org (&optional initial)
  "Search for files with `find' in ~/main/org."
  (interactive "P")
  (consult-find "~/main/org" initial))

;; same for `consult-grep`
(defun ub/consult-grep-main (&optional initial)
  "Search for files with `grep' in ~/main."
  (interactive "P")
  (consult-grep "~/main" initial))
(defun ub/consult-grep-home (&optional initial)
    "Search for files with `grep' in ~/"
    (interactive "P")
    (consult-grep "~/" initial))
(defun ub/consult-grep-org (&optional initial)
    "Search for files with `grep' in ~/main/org."
    (interactive "P")
    (consult-grep "~/main/org" initial))

;; `consult-find` that asks the user dir which can be found interactively in minibuffer
(defun ub/consult-find-interactive ()
  "Search for files with `find' in a directory."
  (interactive)
  (consult-find (read-directory-name "Directory: ")))
;; same for `consult-grep`
(defun ub/consult-grep-interactive ()
  "Search for files with `grep' in a directory."
  (interactive)
  (consult-grep (read-directory-name "Directory: ")))

;; company auto-complete
(elpaca
 (company
  :fetcher github
  :repo "company-mode/company-mode"))
(elpaca-wait)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay nil)
(setq company-minimum-prefix-length 1)
;; company keybinding
(global-set-key (kbd "C-;") #'company-indent-or-complete-common)

;; real-auto-save
(elpaca
 (real-auto-save
  :fetcher github
  :repo "ChillarAnand/real-auto-save"))
(elpaca-wait)
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'org-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 60)


;; vterm
(elpaca
 (vterm
  :fetcher github
  :repo "akermu/emacs-libvterm"))
(elpaca-wait)
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

;; REF modules/term/vterm/autoload.el
(defun +vterm--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let* ((project-root default-directory)
         (default-directory
           (if arg
               default-directory
             project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

(defun +vterm/here (arg)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (interactive "P")
  (+vterm--configure-project-root-and-display
   arg
   (lambda()
     (require 'vterm)
     ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
     (save-window-excursion
       (pop-to-buffer "*scratch*"))
     (let (display-buffer-alist)
       (vterm vterm-buffer-name)))))
(global-set-key (kbd "C-c o T") '+vterm/here)

;; magit
;; when emacs 29 >= install as below
(when (>= emacs-major-version 29)
  ;; built-in versions are too old for magit
  ;; seq and transient are dependencies of magit
  (elpaca
      (seq))
  (elpaca
      (transient
       :fetcher github
       :repo "magit/transient"))
  (elpaca
      (magit
       :fetcher github
       :repo "magit/magit")))
;; when emacs 29 < install as below
(when (< emacs-major-version 29)

  (elpaca
      (magit
       :fetcher github
       :repo "magit/magit")))

(load-file (expand-file-name "commit-emojis.el" user-emacs-directory))

;; python-mode
;; TODO document the workflow eglot, pyright setup and workflow, e.g.,
;; setting up pyrightconfig.json
;; NOTE doom emacs with 28 doesn't work the whole project when jump to def,
;; but emacs 30 w/ default eglot does
;;

;; eglot's resized echo area display too annoying
;; REF https://joaotavora.github.io/eglot/#Eglot-Features
;; REF https://www.reddit.com/r/emacs/comments/16nnlwa/turn_of_eldoc_in_eglot_without_turning_of_symbol/
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-prefer-doc-buffer t)
;; TODO
;; ;; when emacs < 29, install eglot since it is not built-in
;; (when (< emacs-major-version 29)
;;   ;; (elpaca
;;   ;;     (eglot
;;   ;;      :fetcher github
;;   ;;      :repo "joaotavora/eglot"))
;;   ;; (use-package eglot
;;   ;; :elpaca (:inherit elpaca-menu-gnu-devel-elpa)
;;   ;; ;; ...
;;   ;; )
;;   (elpaca eglot)
;;   )

(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    ;;(define-key map-var (kbd "C-c C-s") #'quickrun-shell)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    (define-key map-var (kbd "C-c C-b") 'python-black-region)))

;; TODO get doom utils in modules/lang/python/autoload:
;; +python/open-repl
;; +python/open-ipython-repl
;; add %autoreload for reloading latest changes background
;; REF https://switowski.com/blog/ipython-autoreload/

;; debugging with realgud
(elpaca
 (realgud
  :fetcher github
  :repo "realgud/realgud"))
;; TODO remote debugging setup with realgud:rpdb
(elpaca-wait)
;; disable extra y/n confirmation as we live dangerously 😎
(setq realgud-safe-mode nil)

;; TODO completion-at-point doesn't work out-of-box
;; but pdb-capf setup provides the completion
;; (elpaca
;;  (realgud-ipdb
;;   :fetcher github
;;   :repo "realgud/realgud-ipdb"))
;; (elpaca-wait)

;; completion at pdb debugger buffer
;; minor bug fix, which was related to doom emacs, but who knows it propagates
;; original repo https://github.com/muffinmad/emacs-pdb-capf
;; ugurbolat/emacs-pdb-capf
(elpaca
 (pdb-capf
  :fetcher github
  :repo "ugurbolat/emacs-pdb-capf"))
(elpaca-wait)
;; regular pdb buffer
(add-hook 'pdb-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'pdb-capf nil t)))
;; realgud:pdb buffer
(add-hook 'pdb-track-mode-hook
	  (lambda ()
	    (add-hook 'completion-at-point-functions
		      'pdb-capf nil t)))

;; (when (< emacs-major-version 29)
;;   (elpaca
;;       (jsonrpc
;;        :fetcher github
;;        :repo "emacs-straight/jsonrpc"))
;;   )

(elpaca
 (copilot
  :fetcher github
  :repo "zerolfx/copilot.el"))
(elpaca-wait)
;; (elpaca
;;     (jsonrpc
;;      :fetcher github
;;      :repo "emacs-straight/jsonrpc"))

(use-package copilot
  :hook (
         ;;(prog-mode . copilot-mode)
         ;;(org-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :config
  ;; disable idle delay for displaying instant completions which can be annoying
  (setq copilot-idle-delay nil)
  (setq copilot-indent-offset-warning-disable 't)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :bind (:map prog-mode-map
              ;; C-S-; NOTE C-; good old unintelligent complete
              ("C-:" . 'copilot-complete)))
;; NOTE copilot--start-agent: Node 18+ is required but found 16.2
;; but works with other doom-emacs version, upgrading might break the other setups...

(elpaca
    (yasnippet
     :fetcher github
     :repo "joaotavora/yasnippet"))
(elpaca-wait)
(require 'yasnippet)
(yas-global-mode 1)
(elpaca
    (doom-snippets
     :fetcher github
     :repo "doomemacs/snippets"))
(setq doom-snippets-dir
      (expand-file-name "repos/snippets" elpaca-directory))

;;(load-file (expand-file-name "~/emacs-configs/credentials.el"))
(load-file (expand-file-name "credentials.el" user-emacs-directory))

(elpaca
    (org-ai
     :fetcher github
     :repo "rksm/org-ai"))
(elpaca-wait)
;; (use-package org-ai
;;   ;; TODO how to defer/lazy-load?
;;   ;;:after org-mode
;;   :config
;;   ;;(require 'org-ai)
;;   (add-hook 'org-mode-hook #'org-ai-mode)
;;   (setq org-ai-openai-api-token (ub/load-key-openai-token))
;;   ;; if you are on the gpt-4 beta:
;;   (setq org-ai-default-chat-model "gpt-4")
;;   (setq chatgpt-temperature 0.1) ;; NOTE set 0.75, etc. if you want creativity/hallicunation
;;   ;;(setq org-ai-default-max-tokens 4096)
;;   ;; if you are using yasnippet and want `ai` snippets
;;   (org-ai-install-yasnippets))
;;(package-install 'websocket)
;;(add-to-list 'load-path "path/to/org-ai")
(require 'org)
(require 'org-ai)
(setq org-ai-openai-api-token (ub/load-key-openai-token))
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
(org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets


(defcustom org-ai-explain-math-prompt
  (concat "You are an expert in math."
          "The following shows a math description in latex."
          "Explain each element in the equation step-by-step."
          "List the necessary background knowledge needed to understand the given math expression such as theorems."
          "when prompting mathematical equations, you will use latex where the inline math mode equation has prefix "
	  "and suffix as such $...$ and display math mode equations such as"
          "\\begin{equation}"
          "..."
          "\\end{equation}"
          "When providing answers, avoid warnings/disclaimers/extra recommendations!!!"
          )
  "The template to use for `org-ai-explain-math'."
  :type 'string
  :group 'org-ai)

(defun org-ai-explain-math (start end)
  "Ask ChatGPT explain a code snippet.
`START' is the buffer position of the start of the code snippet.
`END' is the buffer position of the end of the code snippet."
  (interactive "r")
  (org-ai-on-region start end org-ai-explain-math-prompt))


;; activity-watch
;; when emacs 29 >=
(when (>= emacs-major-version 29)
  (elpaca
      (activity-watch-mode
       :fetcher github
       :repo "pauldub/activity-watch-mode")))
;; when emacs 28 <=, switching to ealier commit
(when (< emacs-major-version 29)
  (elpaca
      (activity-watch-mode
       :fetcher github
       :repo "pauldub/activity-watch-mode"
       :tag "1.4.0"
       ;;:ref "ad671767cffd625cd77119b5d2ccef40726e406a"
       )))
(elpaca-wait)
;;(require 'magit-process)
(global-activity-watch-mode)

;;;; docker
;; (elpaca
;;  (docker
;;   :fetcher github
;;   :repo "Silex/docker.el"))

;; pyvenv
(elpaca
    (pyvenv
     :repo "jorgenschaefer/pyvenv"
     :fetcher github))
(elpaca-wait)
(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                ;; remove tramp prefix from pyvenv-virtual-env due to ssh or docker which starts with /ssh: or /docker: and ends with :/
                (setq pyvenv-virtual-env (replace-regexp-in-string "/.*:" "" pyvenv-virtual-env))
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
		(setq realgud--ipdb-command-name (concat pyvenv-virtual-env "bin/python -m ipdb"))
                ;; (setq realgud:pdb-command-name "pyth on -m pdb")
                (setq realgud:pdb-command-name (concat pyvenv-virtual-env "bin/python -m pdb")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
                (setq realgud:pdb-command-name "python -m pdb")))))

;; drag-stuff: meta-up and meta-down
(elpaca
    (drag-stuff
     :repo "rejeep/drag-stuff.el"
     :fetcher github))
(elpaca-wait)
(require 'drag-stuff)
(drag-stuff-global-mode 1)
;;(drag-stuff-define-keys)
(define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
(define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)

;;(setq shell-file-name "/usr/bin/bash")
(setq shell-file-name "/usr/bin/zsh")
;; -i (interactive?) is important for loading .bashrc?
;; REF https://stackoverflow.com/questions/6411121/how-to-make-emacs-use-my-bashrc-file
;;(setq shell-command-switch "-ic")
;; NOTE exec-path-from-shell takes care of that so no need for interactive mode,
;; which can have its own issues
;; REF https://github.com/purcell/exec-path-from-shell?tab=readme-ov-file#making-exec-path-from-shell-faster
(setq shell-command-switch "-c")

;; add environment variable
;; we modify environment variables for dev. mode
(with-eval-after-load 'python
  (setq python-shell-process-environment
	'(
	  ;; disabling breakpoints
	  ;; to ignore breakpoints not to go into pdb mode
	  ;; python's repl is used for running uninterrupted
	  ;; if you want debugging, call explicitly pdb
	  "PYTHONBREAKPOINT=\"0\""
	  ;; disabling jax's gpu memory preallocation which %90 of the mem.
	  "XLA_PYTHON_CLIENT_PREALLOCATE=\"false\""  
	  )
    ))

;; "Ever find that a command works in your shell, but not in Emacs?" - Oh yeah!
(elpaca
    (exec-path-from-shell
     :repo "purcell/exec-path-from-shell"
     :fetcher github))
(elpaca-wait)
(when (memq window-system '(mac ns x)) ;; if you're in the GUI
  ;; You might have already installed exec-path-from-shell
  (require 'exec-path-from-shell)
  ;; Append any paths you would like to import here:
  (dolist (var '(
                 ;;"LD_LIBRARY_PATH" "PYTHONPATH"
                 ;;"SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                 ))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(when (daemonp) ;; if you're in the emacs-client
    (exec-path-from-shell-initialize))
;; NOTE if things slow down, consider setting them as:
;; REF: https://www.reddit.com/r/emacs/comments/f8xwau/hack_replace_execpathfromshell/
;; (setenv "PATH" "/bin:...")
;; (setq exec-path '("/bin" ...)


;; ;; REF https://github.com/tarsius/keychain-environment/blob/main/keychain-environment.el
;; ;; "Keychain is a script that manages ssh-agent and gpg-agent.
;; ;; It is typically run from the shell's initialization file.
;; ;; It allows your shells and cron jobs to share a single ssh-agent and/or gpg-agent."
;; ;;;###autoload
;; (defun keychain-refresh-environment ()
;;   "Set ssh-agent and gpg-agent environment variables.

;; Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
;; and `GPG_AGENT' in Emacs' `process-environment' according to
;; information retrieved from files created by the keychain script."
;;   (interactive)
;;   (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
;;          (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
;;     (list (and ssh
;;                (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
;;                (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
;;           (and ssh
;;                (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
;;                (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
;;           (and gpg
;;                (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
;;                (setenv       "GPG_AGENT_INFO" (match-string 1 gpg))))))
;; (keychain-refresh-environment)


(require 'display-line-numbers)
;; REF lisp/lib/ui.el
;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.

See `display-line-numbers' for what these values mean."
  (interactive)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))

;; tame the crazy popup windows like messages etc.
(elpaca
    (popper
     :repo "karthink/popper"
     :fetcher github))
(elpaca-wait)
(use-package popper
  :config
  (setq elpaca-core-date '(20240310))
  ;;:ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          ;; *Warnings*
          ("\\*Warnings\\*" . hide)
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


(elpaca
    (aggressive-indent
     :repo "Malabarba/aggressive-indent-mode"
     :fetcher github))

(use-package aggressive-indent-mode
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode))


;; vedang/pdf-tools
(elpaca
    (pdf-tools
     :repo "vedang/pdf-tools"
     :fetcher github))
(elpaca-wait)
(pdf-loader-install)

(load-file (expand-file-name "org-mode.el" user-emacs-directory))
