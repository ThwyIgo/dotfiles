(load-file "~/.emacs.d/pkgs/ligature.elc") ;; M-x emacs-lisp-byte-compile
(unless (file-directory-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-mode t)
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups")))
 '(blink-cursor-mode nil)
 '(company-backends
   '(company-bbdb
     (company-semantic company-cmake company-capf company-clang company-files)
     (company-dabbrev-code company-gtags company-etags company-keywords)
     (company-oddmuse company-dabbrev)
     (company-shell company-shell-env company-fish-shell)
     (company-irony-c-headers company-irony company-c-headers)))
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(delete-selection-mode t)
 '(display-line-numbers 'relative)
 '(electric-pair-mode t)
 '(gdb-many-windows t)
 '(global-flycheck-mode t)
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(company-c-headers company-irony-c-headers haskell-mode yasnippet-snippets neotree multiple-cursors lsp-ui lsp-haskell haskell-snippets flycheck emmet-mode dracula-theme company-shell company-irony ace-window))
 '(parens-require-spaces nil)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "ADBO" :family "Fira Code")))))

;; Added by user --------------------------------------------------
;; Install packages on lauch
(require 'cl-lib)
(require 'package)
(package-initialize)
(defvar required-packages
  '(
    company-irony company-shell flycheck haskell-snippets company
    lsp-haskell lsp-ui lsp-mode yasnippet-snippets yasnippet
    emmet-mode neotree ace-window multiple-cursors dracula-theme
    haskell-mode company-irony-c-headers
    )
   "A list of packages to ensure are installed at launch.")
; method to check if all packages are installed
(defun packages-installed-p ()
  (cl-loop for p in required-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))
; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; If non-graphical, change bg color to black. Dracula's blue can be hard to see in a terminal's limited color space.
(unless (display-graphic-p)
    (add-to-list 'default-frame-alist '(background-color . "black")))

;(set-face-attribute 'default nil :height 170)
;(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))
(require 'ido)
(ido-mode t)
(require 'multiple-cursors)
(define-key mc/keymap (kbd "<return>") nil) ;; Makes multiples-cursors RET insert a new line (It exits the mode by default)
(require 'neotree)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil)
;; (sp-pair "'" nil :actions :rem) ;; smartparens
;; (sp-pair "`" nil :actions :rem)
;; (add-hook 'html-mode-hook
;;           (lambda ()
;;             ;; Default indentation is usually 2 spaces, changing to 4.
;;             (set (make-local-variable 'sgml-basic-offset) 4)))

;; Company & flycheck & yasnippet
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
; No delay in showing suggestions.
(setq company-idle-delay 0)
; Use tab key to cycle through suggestions. ('tng' means 'tab and go')
(company-tng-mode)
; Tab for both company and yasnippet
(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(define-key company-mode-map [tab]
  '(menu-item "maybe-company-expand" nil
              :filter (lambda (&optional _)
                        (when (check-expansion)
                          #'company-complete-common))))
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Haskell
; https://github.com/termslang/emacs-haskell-config/blob/master/init.el
(require 'lsp-mode)
(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(require 'haskell-snippets)
;; C/C++
(setq c-basic-offset 4)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'c-mode-common-hook 'irony-server-kill) ;; Fix server not restarting on the second time a file is opened

;; All font-ligature related stuff works only in Emacs 28+
;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))
;; Enable ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode 't)

;; Web development
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; Install tern on your system: $ npm install -g tern
;(add-to-list 'load-path "/path/to/tern/emacs/")
;(autoload 'tern-mode "tern.el" nil t)
;(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;(eval-after-load 'tern
;   '(progn
;      (require 'tern-auto-complete)
;      (tern-ac-setup)))

;; Functions
(defun duplicate-line()
  "Copy the current line to the next line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )
(defun duplicate-line-up()
  "Create a line above the current line and copy the current line to the previous line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (end-of-line 0)
  (open-line 1)
  (forward-line 1)
  (yank)
  )
;; (defun http-server() ;; Ainda em desenvolvimento
;;   "Prompt user to enter dir path, then start a HTTP server with python3 through bash script."
;;   (interactive)
;;   (async-shell-command (format "cd '%s' && python3 -m http.server" (read-directory-name "Directory:")))
;;   )
(defun ryanmarcus/backward-kill-word () ;; Created by Ryan Marcus (https://stackoverflow.com/questions/28221079/ctrl-backspace-in-emacs-deletes-too-much/60826269#60826269)
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

;; Custom keybindings
;; Simple packages
;; (global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key [f8] 'neotree-toggle)
;; multi-cursor
; (C-') To hide all lines with no cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this-dwim) ;; Change HTML tag
(global-set-key (kbd "C-M-<") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M->") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
;; Hooks
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
;; Functions and macros
(global-set-key (kbd "C-c n") 'duplicate-line)
(global-set-key (kbd "C-c p") 'duplicate-line-up)
(global-set-key  [C-backspace]
		 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "C-c <backspace>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Códigos a serem testados
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))
