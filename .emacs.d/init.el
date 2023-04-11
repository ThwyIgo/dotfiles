(setq warning-minimum-level :emergency) ;; Delete or change this line to :warning when editing the file.
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(unless (file-directory-p (concat user-emacs-directory "pkgs"))
  (make-directory (concat user-emacs-directory "pkgs"))) ;; Place extra packages here
(load-directory (concat user-emacs-directory "pkgs"))    ;; M-x emacs-lisp-byte-compile
(defvar backup-directory (concat user-emacs-directory "backups") "Directory where all backup files will be stored.")
(unless (file-directory-p backup-directory)
  (make-directory backup-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-mode t)
 '(auth-source-save-behavior nil)
 '(backup-directory-alist `((".*" \, backup-directory)))
 '(blink-cursor-mode nil)
 '(byte-compile-verbose nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(compile-command "gcc -g -Wall -lm ./")
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" default))
 '(delete-selection-mode t)
 '(display-line-numbers 'relative)
 '(drag-stuff-global-mode t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(gdb-many-windows t)
 '(git-gutter:update-interval 2)
 '(global-flycheck-mode t)
 '(global-git-gutter-mode t)
 '(global-yascroll-bar-mode t)
 '(haskell-interactive-popup-errors nil)
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mode-line-compact 'long)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(drag-stuff git-gutter lsp-java clang-format+ yascroll magit company-box ligature origami haskell-mode yasnippet-snippets neotree multiple-cursors lsp-ui lsp-haskell haskell-snippets flycheck emmet-mode dracula-theme company-shell ace-window))
 '(parens-require-spaces nil)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 135 :width normal :family "Fira Code")))))
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil) ;; C-x < && C-x >
(put 'downcase-region 'disabled nil)

;; Added by user --------------------------------------------------
;; Install packages on lauch
(package-initialize)
(defvar required-packages
  '(
    company-shell company-box company flycheck haskell-snippets lsp-haskell
    lsp-ui lsp-mode lsp-java yasnippet-snippets yasnippet yascroll emmet-mode
    neotree ace-window multiple-cursors dracula-theme haskell-mode origami
    ligature magit git-gutter smex dashboard all-the-icons telephone-line
    nix-mode clang-format+ drag-stuff
    ;;company-nixos-options
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

;; Dracula is horrific in a limited color space. Changing theme when in a
;; non-graphical environment
(if (or (display-graphic-p) (daemonp))
    (load-theme 'dracula)
  (disable-theme 'dracula)
  (load-theme 'manoj-dark)
  )
;; Emacs doesn't take the whole screen when in full screen mode in some window
;; managers. This fixes it.
(setq frame-resize-pixelwise t)
;(set-face-attribute 'default nil :height 170)
;(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil)

;; Simple packages configs
(ido-mode t)
(require 'multiple-cursors)
; Makes multiples-cursors RET insert a new line (It exits the mode by default)
(define-key mc/keymap (kbd "<return>") nil)
(add-hook 'prog-mode-hook
          (lambda () (origami-mode)))
(when (or (display-graphic-p) (daemonp))
  (require 'all-the-icons))
(require 'neotree)
(setq neo-theme (if (or (display-graphic-p) (daemonp)) 'icons 'arrow))
(require 'prolog)
;;(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                              auto-mode-alist))
(require 'git-gutter)
(global-git-gutter-mode t)

;; Dashboard
(require 'dashboard-widgets)
(dashboard-setup-startup-hook)
; Show dashboard in every new emacsclient
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;; Set the title
;(setq dashboard-banner-logo-title "Welcome to Emacs!")
;; Set the banner
;(setq dashboard-startup-banner [VALUE])
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; Content is not centered by default. To center, set
;(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        ))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "bookmark")))
;; To disable shortcut "jump" indicators for each section, set
;(setq dashboard-show-shortcuts nil)

;; Mode line
(setq-default mode-line-format
      '("%e" mode-line-front-space      ;; Small empty space on the right
        mode-line-mule-info             ;; Coding system and end-of-line style
        mode-line-client                ;; Displays "@" when editing in a emacsclient frame
        mode-line-modified              ;; Two "*" indicating if buffer is writable and if it
                                        ;; is modified. It also works as a toggle
        ;mode-line-remote               ;; Indicates if the current directory is local(-) or remote
        mode-line-frame-identification  ;; IDK, it just adds some blank space
        mode-line-buffer-identification ;; Name of the buffer
        "   "                           ;; Blank space
        mode-line-position              ;; Scroll percentage, line number and column number
        (vc-mode vc-mode)               ;; Version control info
        "  "
        mode-line-modes                 ;; Major and minor modes info
        mode-line-misc-info             ;; IDK
        mode-line-end-spaces            ;; Mode line construct to put at the end of the mode line.
        ))

;; Company & flycheck & yasnippet
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
; No delay in showing suggestions.
(setq company-idle-delay 0)
;; ; Add yasnippet support for all company backends
;; ; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;(add-to-list 'company-backends 'company-nixos-options)

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
(define-key company-active-map (kbd "C-TAB") #'company-complete-common)
(define-key company-active-map (kbd "C-<tab>") #'company-complete-common)
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
(yas-global-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Programming languages configs
(require 'lsp-mode)
(require 'lsp)
;; Prevent LSP from removing YASnippet from company
(require 'lsp-completion)
(setq lsp-completion-provider :none)


;; Haskell
; C-c C-l : REPL (haskell-mode)
; https://github.com/termslang/emacs-haskell-config/blob/master/init.el
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(require 'haskell-snippets)
(add-hook 'haskell-mode-hook
          (lambda ()
            (push '("\\" . ?λ) prettify-symbols-alist)))
(add-hook 'haskell-mode-hook 'prettify-symbols-mode 1)

;; C/C++
(setq c-default-style "linux"
      c-basic-offset 4)
(require 'lsp-clangd)
(setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--clang-tidy"))
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'clang-format+-mode)

;; Java
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(defun my-java-mode-hook ()
  ;; (subword-mode) ;; camelCase words are separate words
  ;; (when window-system
  ;;   (set-fringe-style '(8 . 0)))

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Web development
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

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
    (backward-kill-word 1))
  )
(defun insert-random-number (*n) ;; (random t) para que o número seja mais aleatório.
  "Insert *n random digits. *n default to 5.
Call `universal-argument' before for different count."
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount)))))
  )
(defun split-and-follow-horizontally ()
  "Split the selected window into two windows, one above the other.
The selected window is below. The newly split-off window displays
the same buffer."
  (interactive)
  (split-window-below)
  (other-window 1)
  )
;; (defun split-and-follow-vertically ()
;;   "Split the selected window into two side-by-side windows.
;; The selected window is on the right. The newly split-off window
;; displays the same buffer."
;;   (interactive)
;;   (split-window-right)
;;   (other-window 1)
;;   )
(defun asciify-text (&optional Begin End)
  "Remove accents in some letters. e.g. café → cafe.
Change European language characters into equivalent ASCII ones.
When called interactively, work on current line or text selection.

URL `http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html'
Version 2018-11-12 2021-09-17"
  (interactive)
  (let (($charMap
          [
           ["ß" "ss"]
           ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
           ["æ" "ae"]
           ["ç\\|č\\|ć" "c"]
           ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
           ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
           ["ñ\\|ň\\|ń" "n"]
           ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
           ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
           ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
           ["þ" "th"]
           ["ď\\|ð\\|đ" "d"]
           ["ĩ" "i"]
           ["ľ\\|ĺ\\|ł" "l"]
           ["ř\\|ŕ" "r"]
           ["š\\|ś" "s"]
           ["ť" "t"]
           ["ž\\|ź\\|ż" "z"]
           [" " " "]       ; thin space etc
           ["–" "-"]       ; dash
           ["—\\|一" "--"] ; em dash etc
           ])
         ($p1 (if Begin Begin
                (if (region-active-p)
                    (region-beginning)
                  (line-beginning-position))))
         ($p2 (if End End
                (if (region-active-p)
                    (region-end)
                  (line-end-position)))))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $p1 $p2)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (re-search-forward (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

;; Custom keybindings
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)
;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))
;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)
(provide 'my-mode)

;; Simple packages
;; (global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-<return>") 'origami-forward-toggle-node)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; multi-cursor
; (C-') To hide all lines with no cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-S-k") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this-dwim) ;; Change HTML tag
(global-set-key (kbd "C-M-<") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M->") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
;; Hooks
(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))
(add-hook 'prolog-mode-hook
          ((lambda () (define-key prolog-mode-map (kbd "C-c C-c") 'prolog-compile-file))
           ))
;; Functions and macros
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
;(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key (kbd "C-c n") 'duplicate-line)
(global-set-key (kbd "C-S-<up>")  'drag-stuff-up)
(global-set-key (kbd "C-S-<down>")  'drag-stuff-down)
(global-set-key  [C-backspace] 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "C-c <backspace>") 'delete-trailing-whitespace)

;; Override major modes keymaps
(define-key my-mode-map (kbd "C-c c") 'comment-or-uncomment-region)
(define-key my-mode-map (kbd "C-c TAB") 'align-regexp)

(require 'server)
(unless (server-running-p)
  (server-start))
