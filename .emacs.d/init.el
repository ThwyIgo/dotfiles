(setq warning-minimum-level :emergency) ;; Delete or change this line to :warning when editing the file.
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(unless (file-directory-p "~/.emacs.d/pkgs")
  (make-directory "~/.emacs.d/pkgs")) ;; Place extra packages here
(load-directory "~/.emacs.d/pkgs")    ;; M-x emacs-lisp-byte-compile
(unless (file-directory-p "~/.emacs.d/backups")
  (make-directory "~/.emacs.d/backups"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-mode t)
 '(auth-source-save-behavior nil)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups")))
 '(blink-cursor-mode nil)
 '(byte-compile-verbose nil)
 '(column-number-mode t)
 '(company-backends
   '(company-bbdb
     (company-semantic company-cmake company-capf company-clang company-files)
     (company-dabbrev-code company-gtags company-etags company-keywords)
     (company-oddmuse company-dabbrev)
     (company-shell company-shell-env company-fish-shell)
     (company-c-headers)))
 '(compile-command "gcc -g -Wall ./")
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(delete-selection-mode t)
 '(display-line-numbers 'relative)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(gdb-many-windows t)
 '(global-flycheck-mode t)
 '(haskell-interactive-popup-errors nil)
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(magit company-box ligature origami company-c-headers haskell-mode yasnippet-snippets neotree multiple-cursors lsp-ui lsp-haskell haskell-snippets flycheck emmet-mode dracula-theme company-shell ace-window))
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
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 155 :width normal)))))
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil) ;; C-x < && C-x >
(put 'downcase-region 'disabled nil)

;; Added by user --------------------------------------------------
;; Install packages on lauch
(require 'cl-lib)
(require 'package)
(package-initialize)
(defvar required-packages
  '(
    company-shell company-box company flycheck haskell-snippets lsp-haskell
    lsp-ui lsp-mode yasnippet-snippets yasnippet emmet-mode neotree ace-window
    multiple-cursors dracula-theme haskell-mode origami ligature magit
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

(load-theme 'dracula)
;; If non-graphical, change bg color to black. Dracula's blue can be hard to see
;; in a terminal's limited color space. Currently disabled because it doesn't
;; work as expected when running Emacs server
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (unless (display-graphic-p)
;;               (set-face-background 'default "black" nil))))

(setq frame-resize-pixelwise t) ;; Emacs doesn't take the whole screen when in full screen mode in some window managers. This fixes it.
;(set-face-attribute 'default nil :height 170)
;(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))
(ido-mode t)
(require 'multiple-cursors)
(define-key mc/keymap (kbd "<return>") nil) ;; Makes multiples-cursors RET insert a new line (It exits the mode by default)
(require 'neotree)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil)
;; (add-hook 'html-mode-hook
;;           (lambda ()
;;             ;; Default indentation is usually 2 spaces, changing to 4.
;;             (set (make-local-variable 'sgml-basic-offset) 4)))
(add-hook 'prog-mode-hook
          (lambda () (origami-mode)))

;; Company & flycheck & yasnippet
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
; No delay in showing suggestions.
(setq company-idle-delay 0)
; Tab for both company and yasnippet
(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(dolist (key '("<return>" "RET"))
    ;; Here we are a feature of define-key that lets us pass an "extended menu
    ;; item" instead of an interactive function. Doing this allows RET to regain
    ;; its usual functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)
  ;; Company appears to override the above keymap based on company-auto-complete-chars.
  ;; Turning it off ensures we have full control.
  (setq company-auto-complete-chars nil)
;; (define-key company-mode-map [tab]
;;   '(menu-item "maybe-company-expand" nil
;;               :filter (lambda (&optional _)
;;                         (when (check-expansion)
;;                           #'company-complete-common))))
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Haskell
; C-c C-l : REPL (haskell-mode)
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
(setq c-default-style "linux"
      c-basic-offset 4)
(add-hook 'c-mode-hook #'lsp)

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

;; Functions
(defun bg-black()
  "Set the frame's background color black."
  (interactive)
  (set-background-color "black")
  )
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
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  ;(indent-according-to-mode)
  )
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  ;(indent-according-to-mode)
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
(defun insert-random-number (*n) ;; (random t) para que o número seja mais aleatório.
  "Insert *n random digits. *n default to 5.
Call `universal-argument' before for different count."
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount))))))

;; Custom keybindings
;; Simple packages
;; (global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-<return>") 'origami-forward-toggle-node)
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
(global-set-key (kbd "C-S-<up>")  'move-line-up)
(global-set-key (kbd "C-S-<down>")  'move-line-down)
(global-set-key  [C-backspace] 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "C-c <backspace>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c TAB") 'align-current)

;; Códigos a serem testados
(require 'server)
(unless (server-running-p)
  (server-start))
