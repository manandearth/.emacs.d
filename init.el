(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/")
;;(load-theme 'zenburn t)
(use-package color-theme-sanityinc-solarized :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)

(require 'color-theme-sanityinc-tomorrow)
(require 'sanityinc-tomorrow-night-theme)
;;(load-theme 'color-theme-sanityinc-tomorrow)


;; switching themes with keys asigned
(defun theme-clojure ()
  (interactive)
   (load-theme 'sanityinc-tomorrow-eighties t))

;;(set-face-attribute 'default t :font "dejavu sans mono-15")

(defun theme-org ()
  (interactive)
   ;;theme recommended by toxi for LP
  (load-theme 'sanityinc-solarized-light t))

;;global font size
;;(set-face-attribute 'default t :font "dejavu sans mono-15")
(set-face-font 'default "dejavu sans mono-15")
;;emojies
(set-fontset-font t 'unicode "Emoji One Color" nil 'prepend)
 

;;this is re-stated for emacs client further down.
;;start light
(theme-clojure)

(global-set-key (kbd "C-c t c") 'theme-clojure)
(global-set-key (kbd "C-c t o") 'theme-org)

(require 'transpose-frame)
(require 'ob-ipython)
(require 'conda)
(require 'ob-sql-mode)
(require 'web-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook 'company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'lpy-mode)

;; Make cursor more visible when you move a long distance
(use-package beacon
  :config
  (beacon-mode 1))

(autoload 'ivy-bibtex "ivy-bibtex" "" t)

(defvar myPackages
  '(better-defaults
    ein ;; add the ein (Emacs ipython notebook)
    elpy ;; add the elpy package
    flycheck ;; add the flycheck package
    conda
    py-autopep8
    paredit ;; makes handling lisp expressions much, much easier
    clojure-mode ;; key bindings and code colorization for Clojure
    clojure-mode-extra-font-locking  ;; extra syntax highlighting for clojure
    cider  ;; integration with a Clojure REPL
    ;;ido-ubiquitous ;;allow ido usage in as many contexts as possible
    projectile 
    rainbow-delimiters  ;; colorful parenthesis matching
    tagedit   ;; edit html tags like sexps
))

(dolist (p myPackages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'scimax-org-babel-ipython)

;;(add-to-list 'load-path "~/git/swiper/") (require 'ivy) 
;;(ivy-mode 1) 
;; for new users:
;;(setq ivy-use-virtual-buffers t) (setq ivy-count-format "(%d/%d) ") 
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'ido) ;;interactively do things...
;;(ido-mode t)

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)
;; (defvar predicate nil)
;; (defvar inherit-input-method nil)
(require 'flx-ido)
     (ido-mode 1)
    (ido-everywhere 1)
     (flx-ido-mode 1)
;;     ;; disable ido faces to see flx highlights.
     (setq ido-enable-flex-matching t)
     (setq ido-use-faces nil)
;;Ivy is easier to read..:
(setq projectile-completion-system 'ivy)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(pdf-tools-install)
(setq pdf-view-use-unicode-ligther nil)

;;(elpy-enable)
;; (elpy-use-ipython)

;; (when (require 'flycheck nil t)
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (add-hook 'anaconda-mode-hook 'flycheck-mode)

;; (require 'py-autopep8)
;; (add-hook 'anaconda-mode-hook 'py-autopep8-enable-on-save)

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

(setq python-shell-interpreter "/home/adam/anaconda3/bin/ipython3")
(setq ob-ipython-command "/home/adam/anaconda3/bin/jupyter")

(defun python-mode-outline-hook ()
  (setq outline-level 'python-outline-level)

  (setq outline-regexp
    (rx (or
         ;; Commented outline heading
         (group
          (* space)  ; 0 or more spaces
          (one-or-more (syntax comment-start))
          (one-or-more space)
          ;; Heading level
          (group (repeat 1 8 "\*"))  ; Outline stars
          (one-or-more space))

         ;; Python keyword heading
         (group
          ;; Heading level
          (group (* space)) ; 0 or more spaces
          bow
          ;; Keywords
          (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
          eow)))))

(defun python-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
               (* space)
               (one-or-more (syntax comment-start))
               (one-or-more space)
               (group (one-or-more "\*"))
               (one-or-more space))
              (match-string 0))
    (- (match-end 0) (match-beginning 0)))

   ;; Python keyword heading, set by number of indentions
   ;; Add 8 (the highest standard outline level) to every Python keyword heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

(add-hook 'python-mode-hook 'python-mode-outline-hook)

(setq scimax-dir "/home/adam/scimax/")
(add-to-list 'load-path "/home/adam/scimax/") ;; TODO find how to require from scimax

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package indium
  :ensure t
  :diminish (indium-interaction-mode . "In" )
  :init
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package simple-httpd
  :ensure t)

(require 'ledger-mode)

(require 'ob-latex)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1)))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)

(setq evil-default-state 'emacs)    ;starts in emacs-state (C-z to toggle)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1) ;positively start evil-mode when starting new buffer

(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'paren)
;;(load-library "paren")
(show-paren-mode 1)
(transient-mark-mode t)

(global-set-key (kbd "C-x ;") 'comment-line)

(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'lisp-interaction-mode (lambda() (rainbow-delimiters-mode 1)))
(add-hook 'clojure-mode (lambda() (rainbow-delimiters-mode 1)))
;;(global-rainbow-delimiters-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p nil)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

;;custom face to fit more..:
(set-face-attribute 'which-key-command-description-face nil :font "dejavu sans mono-15" :inherit nil)
(set-face-attribute 'which-key-key-face nil :font "dejavu sans mono-15" :inherit nil)

(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
;;and swap:
(global-set-key (kbd "C-x \\") 'window-swap-states)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;(setq aw-background nil) ;to disable the dimming of window for visibility of key char.
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
	(?m aw-swap-window "Swap Windows")
	(?M aw-move-window "Move Window")
	(?j aw-switch-buffer-in-window "Select Buffer")
	(?n aw-flip-window)
	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	(?c aw-split-window-fair "Split Fair Window")
	(?v aw-split-window-vert "Split Vert Window")
	(?b aw-split-window-horz "Split Horz Window")
	(?o delete-other-windows "Delete Other Windows")
	(?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(require 'move-text)
(move-text-default-bindings)

(setq kill-whole-line t)

;; delete space upto next word with `M-D` (from emacs wiki)
(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) 
		     (progn (skip-chars-forward " \t") (point))))
(global-set-key (kbd "M-D") 'delete-horizontal-space-forward)


;;and backwards.. bind to M-backspace [M-del]
;;C-backspace is still backward-kill-word 
(defun backward-delete-char-hungry (arg &optional killp)
      "*Delete characters backward in \"hungry\" mode.\n    See the documentation of `backward-delete-char-untabify' and\n    `backward-delete-char-untabify-method' for details."
      (interactive "*p\nP")
      (let ((backward-delete-char-untabify-method 'hungry))
        (backward-delete-char-untabify arg killp)))

(global-set-key [M-del] 'backward-delete-char-hungry)

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))

(require 'iedit)
(use-package iedit
  :config (set-face-background 'iedit-occurrence "Magenta"))

(global-set-key (kbd "C-:") 'iedit-mode)

(when (fboundp 'winner-mode)
      (winner-mode 1))

 (require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.mp4\\'" "vlc" (file))))

;;(require 'golden-ratio)
;;(golden-ratio-mode 1)
;;(add-hook 'org-agenda-hook (lambda () (golden-ratio-mode -1)))

(global-set-key (kbd "M-+") 'hs-show-block)
(global-set-key (kbd "M-*") 'hs-show-all)
(global-set-key (kbd "M--") 'hs-hide-block)
(global-set-key (kbd "M-Ç") 'hs-hide-level)
(global-set-key (kbd "M-:") 'hs-hide-all)

(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'cider-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(save-place-mode 1)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

 (add-hook 'term-setup-hook
  '(lambda ()
     (define-key function-key-map "\e[1;5A" [C-up])
     (define-key function-key-map "\e[1;5B" [C-down])
     (define-key function-key-map "\e[1;5C" [C-right])
     (define-key function-key-map "\e[1;5D" [C-left])))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
    (tool-bar-mode -1))))
(set-face-font 'default "dejavu sans mono-15"))

;; For the case that the init file runs after the frame has been created.
;; Call of emacs without --daemon option.
(my-frame-tweaks) 
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)

(global-set-key (kbd "C-c i")
(lambda() (interactive)(org-babel-load-file "~/.emacs.d/init.org")))

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key esc-map "." #'xref-find-definitions)

; status globally
(global-set-key (kbd "C-x g") 'magit-status)
; pop up of pop ups globally
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Specify the jupyter executable name, and the start dir of the server
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "/home/adam/notebooks/")

(add-to-list 'load-path "~/.emacs.d/lpy/")
(require 'lpy)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(add-hook 'clojure-mode-hook 'subword-mode)

(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;; provides minibuffer documentation for the code you're typing into the repl
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;;comes with chestnut
;; (setq cider-default-cljs-repl
;;       "(do (user/go)
;;            (user/cljs-repl))")

;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl))")

;; https://github.com/stuartsierra/component/issues/55
 ;; emacs, init.el

;;  ;; find all buffers names which match `reg`, regex
;;  (defun find-buffer-regex (reg)
;;    (interactive)
;;    (remove-if-not #'(lambda (x) (string-match reg x))
;;  		   (mapcar #'buffer-name (buffer-list))))

;; ;;define executing a command in an open nrepl
;;  (defun cider-execute (command)
;;   (interactive)
;;   (set-buffer (car (find-buffer-regex "cider-repl.*")))
;;   (goto-char (point-max))
;;   (insert command)
;;   (cider-repl-return))

;; ;;nrepl reset for refreshing namespaces
;; (defun nrepl-reset ()
;;   (interactive)
;;   (cider-execute "(clojure.tools.namespace.repl/refresh)"))

;;   (define-key cider-mode-map (kbd "C-c r") 'nrepl-reset)

;;   (define-key cider-repl-mode-map (kbd "C-c r") 'nrepl-reset)

;;nrepl <ret> and <ctrl-ret>, newline and eval respectively...
;; (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
;; (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-return)

;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(global-set-key (kbd "C-x b") 'ibuffer)
(setq ibuffer-expert t)

;;attempt:

;;(add-hook 'ibuffer-mode-hook
;;	  (lambda ()
;;	      (ibuffer-switch-to-saved-filter-groups "default")))

(use-package avy
  :ensure t
  :bind
    ("M-s" . avy-goto-char))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

;; Focus on *Occur* window right away.
(add-hook 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
    (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body) ))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("j" occur-next "Next" :color red)
  ("k" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))

(global-set-key (kbd "C-o") 'hydra-occur-dwim/body)

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("M" ibuffer-set-filter-groups-by-mode "groups-by-mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(global-set-key
 (kbd "C-c f")
 (defhydra hydra-folding (:color red)
   "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)))

;;; Code:

(require 'cider-apropos)
(require 'cider-client)
(require 'cider-doc)
(require 'cider-grimoire)
;; (require 'cider-interaction) ;disappeared
(require 'cider-macroexpansion)
(require 'cider-mode)
(require 'cider-repl)
(require 'cider-test)
(require 'cider-inspector)
(require 'hydra)

;;;; Customize

(defgroup cider-hydra nil
  "Hydras for CIDER."
  :prefix "cider-hydra-"
  :group 'cider)

;;;; Documentation

(defhydra cider-hydra-doc (:color blue)
  "
CIDER Documentation
---------------------------------------------------------------------------
_d_: CiderDoc                           _j_: JavaDoc in browser
_a_: Search symbols                     _s_: Search symbols & select
_A_: Search documentation               _e_: Search documentation & select
_r_: Grimoire                           _h_: Grimoire in browser
"
  ;; CiderDoc
  ("d" cider-doc nil)
  ;; JavaDoc
  ("j" cider-javadoc nil)
  ;; Apropos
  ("a" cider-apropos nil)
  ("s" cider-apropos-select nil)
  ("A" cider-apropos-documentation nil)
  ("e" cider-apropos-documentation-select nil)
  ;; Grimoire
  ("r" cider-grimoire nil)
  ("h" cider-grimoire-web nil))


;;;; Loading and evaluation

(defhydra cider-hydra-eval (:color blue)
  "
CIDER Evaluation
---------------------------------------------------------------------------
_k_: Load (eval) buffer                 _l_: Load (eval) file
_p_: Load all project namespaces
_r_: Eval region                        _n_: Eval ns form
_e_: Eval last sexp                     _p_: Eval last sexp and pprint
_w_: Eval last sexp and replace         _E_: Eval last sexp to REPL
_d_: Eval defun at point                _f_: Eval defun at point and pprint
_:_: Read and eval                      _i_: Inspect
_m_: Macroexpand-1                      _M_: Macroexpand all
"
  ;; Load
  ("k" cider-load-buffer nil)
  ("l" cider-load-file nil)
  ("p" cider-load-all-project-ns nil)
  ;; Eval
  ("r" cider-eval-region nil)
  ("n" cider-eval-ns-form nil)
  ("e" cider-eval-last-sexp nil)
  ("p" cider-pprint-eval-last-sexp nil)
  ("w" cider-eval-last-sexp-and-replace nil)
  ("E" cider-eval-last-sexp-to-repl nil)
  ("d" cider-eval-defun-at-point nil)
  ("f" cider-pprint-eval-defun-at-point nil)
  (":" cider-read-and-eval nil)
  ;; Inspect
  ("i" cider-inspect nil)
  ;; Macroexpand
  ("m" cider-macroexpand-1 nil)
  ("M" cider-macroexpand-all nil))

;;;; Testing and debugging

(defhydra cider-hydra-test (:color blue)
  "
CIDER Debug and Test
---------------------------------------------------------------------------
_x_: Eval defun at point
_v_: Toggle var tracing                 _n_: Toggle ns tracing
_t_: Run test                           _l_: Run loaded tests
_p_: Run project tests                  _r_: Rerun tests
_s_: Show test report
"
  ;; Debugging
  ("x" (lambda () (interactive) (cider-eval-defun-at-point t)) nil)
  ("v" cider-toggle-trace-var nil)
  ("n" cider-toggle-trace-ns nil)
  ;; Testing
  ("t" cider-test-run-test nil)
  ("l" cider-test-run-loaded-tests nil)
  ("r" cider-test-rerun-failed-tests nil)
  ("p" cider-test-run-project-tests nil)
  ("s" cider-test-show-report nil))

;;;; REPL

(defhydra cider-hydra-repl (:color blue)
  "
CIDER REPL
---------------------------------------------------------------------------
_d_: Display connection info            _r_: Rotate default connection
_z_: Switch to REPL                     _n_: Set REPL ns
_p_: Insert last sexp in REPL           _x_: Reload namespaces
_o_: Clear REPL output                  _O_: Clear entire REPL
_b_: Interrupt pending evaluations      _Q_: Quit CIDER
"
  ;; Connection
  ("d" cider-display-connection-info nil)
  ("r" cider-rotate-default-connection nil)
  ;; Input
  ("z" cider-switch-to-repl-buffer nil)
  ("n" cider-repl-set-ns nil)
  ("p" cider-insert-last-sexp-in-repl nil)
  ("x" cider-refresh nil)
  ;; Output
  ("o" cider-find-and-clear-repl-output nil)
  ("O" (lambda () (interactive) (cider-find-and-clear-repl-output t)) nil)
  ;; Interrupt/quit
  ("b" cider-interrupt nil)
  ("Q" cider-quit nil))

;;;; Key bindings and minor mode

(defvar cider-hydra-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-mode-map)
    (define-key map (kbd "C-c C-d") #'cider-hydra-doc/body)
    (define-key map (kbd "C-c C-t") #'cider-hydra-test/body)
    (define-key map (kbd "C-c M-t") #'cider-hydra-test/body)
    (define-key map (kbd "C-c M-r") #'cider-hydra-repl/body)
    map)
  "Keymap for CIDER hydras.")

;;;###autoload
(define-minor-mode cider-hydra-mode
  "Hydras for CIDER."
  :keymap cider-hydra-map
  :require 'cider)

(provide 'cider-hydra)
;;; cider-hydra.el ends here

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/") ;;mu4e mail
(require 'mu4e)
(require 'smtpmail)
(use-package mu4e-alert
 :ensure t
 :after mu4e
 :init
 (setq mu4e-alert-interesting-mail-query
   (concat
    "flag:unread maildir:/INBOX "
    ))
 (mu4e-alert-enable-mode-line-display)
 (defun gjstein-refresh-mu4e-alert-mode-line ()
   (interactive)
  (mu4e~proc-kill)
   (mu4e-alert-enable-mode-line-display)
   )
 (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
 )

(setq mu4e-get-mail-command "offlineimap")

;; tell message-mode how to send mail
;; (setq message-send-mail-function 'smtpmail-send-it)

;; (setq smtpmail-smtp-server "smtp.posteo.de")

;; close message buffer
(setq message-kill-buffer-on-exit t)

;; attachments go here:
(setq mu4e-attachment-dir  "~/Downloads")

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; my name and user
;; (setq user-mail-address "adam@manandearth.net"
      ;; user-full-name  "Adam Gefen")

;; intervals between updating the incoming mail
(setq mu4e-update-interval 300)
;; for nullmailer:
;; (setq message-send-mail-function 'message-send-mail-with-sendmail)

; I have my "default" parameters
(setq mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      message-send-mail-function 'smtpmail-send-it
      user-mail-address "adamgefen@posteo.net"
      user-full-name  "Adam Gefen"
      smtpmail-default-smtp-server "smtp.posteo.de"
      smtpmail-smtp-server "posteo.de"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

;; convenience function for starting the whole mu4e in its own frame
;; posted by the author of mu4e on the mailing list
(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(defun mbork/message-attachment-present-p ()
  "Return t if an attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part" nil t) t))))

(defcustom mbork/message-attachment-intent-re
  (regexp-opt '("I attach"
		"I have attached"
		"I've attached"
		"I have included"
		"I've included"
		"see the attached"
		"see the attachment"
		"attached file"
		"attached"))
  "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

(defcustom mbork/message-attachment-reminder
  "Are you sure you want to send this message without any attachment? "
  "The default question asked when trying to send a message
containing `mbork/message-attachment-intent-re' without an
actual attachment.")

(defun mbork/message-warn-if-no-attachments ()
  "Ask the user if s?he wants to send the message even though
there are no attachments."
  (when (and (save-excursion
	       (save-restriction
		 (widen)
		 (goto-char (point-min))
		 (re-search-forward mbork/message-attachment-intent-re nil t)))
	     (not (mbork/message-attachment-present-p)))
    (unless (y-or-n-p mbork/message-attachment-reminder)
      (keyboard-quit))))

(add-hook 'message-send-hook #'mbork/message-warn-if-no-attachments)

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

;;First the fillparagraph value can be defined like so:
;;C-u <number of char per line> C-x f
;;followed by leuven-good-old-fill-paragraph
;;or unfill-paragraph  (M-q or M-Q)

(defun leuven-good-old-fill-paragraph ()
  (interactive)
  (let ((fill-paragraph-function nil)
        (adaptive-fill-function nil))
    (fill-paragraph)))
(define-key org-mode-map "\M-q" 'leuven-good-old-fill-paragraph)

;;; This is the opposite of fill-paragraph    
;;;from https://www.emacswiki.org/emacs/FillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;; Handy key definition
    (define-key global-map "\M-Q" 'unfill-paragraph)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)(setq org-directory "~/notes")
(setq org-default-notes-file "~/notes/refile.org")

;;this I reseted since starting to use solarized theme
;;previously with Leuven org-level-1 :height was 1.5
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Keep org-mode timestamps in English, e.g. [2016-11-05 Sat 10:03]
(setq system-time-locale "C")

(setq org-hide-emphasis-markers t)

(add-hook 'org-mode-hook
               (lambda ()
                 (define-key org-mode-map "\C-csv"
                             'org-table-export)))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files (quote ("~/notes"
                               "~/notes/study/")))

(setq org-ref-notes-directory "~/notes/ref/"
      org-ref-bibliography-notes "~/notes/ref/index.org"
      org-ref-default-bibliography '("~/notes/ref/index.bib")
      org-ref-pdf-directory "~/notes/ref/lib/")

;; Enable org export to odt (OpenDocument Text)
;; It is disabled by default in org 8.x
(eval-after-load "org"
'(require 'ox-odt nil t))

;; email links to mu4e
(require 'org-mu4e)

;; For allowing refile to work between files stored in org-agenda-files variable.
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "POSTPONED(p@/!)" "DELEGATED(e@/!)" "|" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "pink" :weight bold)
              ("NEXT" :foreground "light blue" :weight bold)
              ("SOMEDAY" :foreground "dark grey" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "light green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)
              ("DELEGATED" :foreground "brown" :weight bold)
	          ("POSTPONED" :foreground "light grey" :weight bold))))

;; fast todo selection, Use C-c C-t KEY (t, n, d)
(setq org-use-fast-todo-selection t)

;; this one is for quick cycling todo modes with S-left and S-right
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; refile in org:

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
;  "Exclude todo keywords with a done state from refile targets"
(not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil)))
		nil)))))

(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

(unless (package-installed-p 'ob-http)
  (package-install 'ob-http))
(require 'ob-http)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)
   (ledger . t)
   (latex . t)
   (clojure .t)
   (shell .t)
   (sql .t)
   (http .t)
    ))

(setq org-src-fontify-natively t
    org-src-preserve-indentation t
    org-src-tab-acts-natively t)

;; all python code be safe
(defun my-org-confirm-babel-evaluate (lang body)
(not (string= lang "python")))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-babel-python-command "/home/adam/anaconda3/bin/ipython3 --no-banner --classic --no-confirm-exit")

;; use %cpaste to paste code into ipython in org mode
(defadvice org-babel-python-evaluate-session
(around org-python-use-cpaste
        (session body &optional result-type result-params) activate)
        "Add a %cpaste and '--' to the body, so that ipython does the right thing."
(setq body (concat "%cpaste\n" body "\n--"))
ad-do-it
(if (stringp ad-return-value)
  (setq ad-return-value (replace-regexp-in-string "\\(^Pasting code; enter '--' alone on the line to stop or use Ctrl-D\.[\r\n]:*\\)" "" ad-return-value))))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

(setq org-babel-default-header-args:python
      '((:results . "output replace")
	(:session . "none")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Useful keybindings when using Clojure from Org
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

;; No timeout when executing calls on Cider via nrepl
(setq org-babel-clojure-sync-nrepl-timeout nil)

;; add <p for python expansion
(add-to-list 'org-structure-template-alist
	     '("p" "#+BEGIN_SRC python :results output org drawer\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
	     '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

;; add <pv for python expansion with value
(add-to-list 'org-structure-template-alist
	     '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
	       "<src lang=\"emacs-lisp\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
	     '("ell" "#+BEGIN_SRC emacs-lisp :lexical t\n?\n#+END_SRC"
	       "<src lang=\"emacs-lisp\">\n?\n</src>"))

;; add <sh for shell
(add-to-list 'org-structure-template-alist
	     '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"
	       "<src lang=\"shell\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
	     '("lh" "#+latex_header: " ""))

(add-to-list 'org-structure-template-alist
	     '("lc" "#+latex_class: " ""))

(add-to-list 'org-structure-template-alist
	     '("lco" "#+latex_class_options: " ""))

(add-to-list 'org-structure-template-alist
	     '("ao" "#+attr_org: " ""))

(add-to-list 'org-structure-template-alist
	     '("al" "#+attr_latex: " ""))

(add-to-list 'org-structure-template-alist
	     '("ca" "#+caption: " ""))

(add-to-list 'org-structure-template-alist
	     '("tn" "#+tblname: " ""))

(add-to-list 'org-structure-template-alist
	     '("n" "#+name: " ""))

(add-to-list 'org-structure-template-alist
	     '("o" "#+options: " ""))

(add-to-list 'org-structure-template-alist
	     '("ti" "#+title: " ""))

;; This causes ERC to connect to the Freenode network upon hitting
;; C-c e f.  Replace MYNICK with your IRC nick.
(global-set-key "\C-cef" (lambda () (interactive)
			   (erc :server "irc.freenode.net" :port "6667"
				:nick "manandearth")))

;; This causes ERC to connect to the IRC server on your own machine (if
;; you have one) upon hitting C-c e b.  Replace MYNICK with your IRC
;; nick.  Often, people like to run bitlbee (http://bitlbee.org/) as an
;; AIM/Jabber/MSN to IRC gateway, so that they can use ERC to chat with
;; people on those networks.
(global-set-key "\C-ceb" (lambda () (interactive)
			   (erc :server "localhost" :port "6667"
				:nick "manandearth")))

;; Make C-c RET (or C-c C-RET) send messages instead of RET. This has
;; been commented out to avoid confusing new users.
;; (define-key erc-mode-map (kbd "RET") nil)
;; (define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
;; (define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

     ;;; Options

;; Join the #emacs and #erc channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc" "#clojure" "#clojure-begginers" "#clojure-emacs")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

(require 'forecast)
(setq calendar-latitude 36.25
      calendar-longitude -5.966667
      calendar-location-name "Vejer de la Frontera, Spain"
      forecast-api-key "c0617e8ff49d67d1a95e1be105225a82")

(setq inhibit-startup-message t) ;; hide the startup message

;; (global-linum-mode t) ;; enable line numbers globally

(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

(if window-system
    (progn (tool-bar-mode -1)
	   (scroll-bar-mode 0)))

(setq visible-bell t); Flashes on error
