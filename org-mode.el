

(use-package org
  :config
  (setq
   org-startup-with-inline-images t
   ;;(setq org-startup-with-latex-preview t)=
   org-startup-folded nil
   org-checkbox-hierarchical-statistics nil
   org-tags-column 0
   org-enforce-todo-dependencies nil
   org-log-state-notes-into-drawer nil
   +org-startup-with-animated-gifs 't
   org-startup-folded nil
   org-edit-src-content-indentation 0
   org-startup-indented t
   org-src-tab-acts-natively t
   org-src-fontify-natively t
   org-fontify-quote-and-verse-blocks t
   org-confirm-babel-evaluate nil
   org-support-shift-select nil
   shift-select-mode nil
   org-list-demote-modify-bullet
       '(("+" . "*") ("*" . "-") ("-" . "+"))
   ))


;; rexim/org-cliplink
(elpaca
    (org-cliplink
     :fetcher github
     :repo "rexim/org-cliplink"))


;; babel python
(use-package org
  :config
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "python-default")
          (:python . "python3")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:async . "yes")
          (:eval . "never-export")))
  (add-to-list 'org-babel-load-languages '(python . t)))

;; emacs-jupyter/jupyter
;;(setq native-comp-jit-compilation-deny-list '("jupyter"))
(elpaca
    (jupyter
     :fetcher github
     ;;:build (:not compile)
     ;;:build (:not elpaca--byte-compile)
     :repo "emacs-jupyter/jupyter"))
(elpaca-wait)
(add-to-list 'load-path (expand-file-name "repos/jupyter/" elpaca-directory))
             ;;"~/emacs-configs/elpaca-emacs-vanilla-really-not-really_29.1/repos/jupyter/")
(add-to-list 'load-path (expand-file-name "repos/emacs-web-server/" elpaca-directory))
             ;;"~/emacs-configs/elpaca-emacs-vanilla-really-not-really_29.1/repos/emacs-web-server")
(add-to-list 'load-path (expand-file-name "repos/emacs-websocket/" elpaca-directory))
             ;;"~/emacs-configs/elpaca-emacs-vanilla-really-not-really_29.1/repos/emacs-websocket")
(add-to-list 'load-path (expand-file-name "repos/emacs-zmq/" elpaca-directory))
             ;;"~/emacs-configs/elpaca-emacs-vanilla-really-not-really_29.1/repos/emacs-zmq")
(require 'ob-jupyter)
(require 'jupyter)
;; BUG zmq.error.ZMQError: Address already in use
(setq jupyter-use-zmq nil) 


(setq org-babel-default-header-args:jupyter
      '((:results . "both")
        (:session . "jupyter-python-default")
        (:kernel . "python3")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:async . "yes")
        (:eval . "never-export")))

(add-to-list 'org-babel-load-languages '(python . t))
(add-to-list 'org-src-lang-modes '("jupyter" . python))

;; syntax highlighting of python for jupyter
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (jupyter . t)))

;; BUG latest Jupyter : REST API error: 404, "Not found" #500
;; mamba install jupyter_server=1.23.4

