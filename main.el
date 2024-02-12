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
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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


;; traditional strip-down
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; good defaults
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t) ; open scratch buffer at startup
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
(load-theme 'modus-vivendi-tinted t)
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
 (nano-modeline
  ;; Inherited from elpaca-menu-item.
  ;;:files (:defaults)
  :fetcher github
  :repo "rougier/nano-modeline"))
(elpaca-wait)
(require 'nano-modeline)
(add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook #'nano-modeline-text-mode)
(setq nano-modeline-position #'nano-modeline-footer)
(setq-default mode-line-format nil)

(elpaca
 (hl-todo
  :fetcher github
  :repo "tarsius/hl-todo"))
(elpaca-wait)
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
        ("REF" . "#660066")))


(recentf-mode 1)

(setq delete-by-moving-to-trash 't)

;; eglot+tramp remote setup
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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
(require 'dired)
(setq delete-by-moving-to-trash 't)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

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


;; keybindings for consult
(global-set-key (kbd "C-c f r") 'consult-recent-file)
(global-set-key (kbd "C-x C-v") 'consult-buffer)
(global-set-key (kbd "C-x C-'") 'consult-grep)
(global-set-key (kbd "C-s") 'consult-line)

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

;; magit
(elpaca
 (magit
  :fetcher github
  :repo "magit/magit"))


;; python-mode
(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    ;;(define-key map-var (kbd "C-c C-s") #'quickrun-shell)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    (define-key map-var (kbd "C-c C-b") 'python-black-region)))

;; debugging with realgud
(elpaca
 (realgud
  :fetcher github
  :repo "realgud/realgud"))
;; TODO remote debugging setup with realgud:rpdb


;; (elpaca
;;   (jsonrpc
;;    :fetcher github
;;    :repo "emacs-straight/jsonrpc"))

;; (elpaca
;;  (copilot
;;   :fetcher github
;;   :repo "zerolfx/copilot.el"))
;; (elpaca-wait)
;; (use-package copilot
;;   :hook (
;;          (prog-mode . copilot-mode)
;;          (org-mode . copilot-mode))
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word))
;;   :bind (:map prog-mode-map
;;               ;; C-S-; NOTE C-; good old unintelligent complete
;;               ("C-:" . 'copilot-complete)))
;; TODO copilot--start-agent: Node 18+ is required but found 16.2
;; but works with other doom-emacs version, upgrading might break the other setups...

;; activity-watch
(elpaca
 (activity-watch-mode
  :fetcher github
  :repo "pauldub/activity-watch-mode"))
(elpaca-wait)
(require 'magit-process)
(global-activity-watch-mode)

;; docker
(elpaca
 (docker
  :fetcher github
  :repo "Silex/docker.el"))

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
(drag-stuff-define-keys)

