;; Emacs 28

(setq warning-minimum-level :emergency) ;; Delete or change this line to :warning when editing the file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p (concat user-emacs-directory "custom.el"))
  (load custom-file))

;;;;; Configs that don't depend on any package ;;;;;

(defvar backup-directory (concat user-emacs-directory "backups") "Directory where all backup files will be stored.")
(unless (file-directory-p backup-directory)
  (make-directory backup-directory))
(setq backup-directory-alist `((".*" \, backup-directory)))

;; Default shell
(setq shell-file-name "fish")

;; Disable tool-bar and menu-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Customize scratch buffer
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; Enable tab-bar (prefix: C-x t)
(setq tab-bar-show 1) ;; Show the tab-bar only when there's more than 2 tabs
(setq tab-bar-close-button-show nil)
(tab-bar-mode)

;; Emacs doesn't take the whole screen when in full screen mode in some window
;; managers. This fixes it.
(setq frame-resize-pixelwise t)

;; Make frame transparent when unfocused
(add-to-list 'default-frame-alist '(alpha . (100 . 85)))

;; Disable some auto-indent features that drive me crazy
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil)

;; By default, Emacs considers a '.' followed by 2 spaces a sentence end.
;; Because 1 space is enough in modern times, I'm changing this variable.
(setq sentence-end-double-space nil)

;; Relative line numbers can help in "C-u NUM C-n" scenarios
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Automatically delete selection after starting to type.
(delete-selection-mode 1)

;; GNU standard
(setq-default fill-column 80)

;; Make I-search show match numbers in the search prompt
(setq isearch-lazy-count t)

;; Please, don't make noise when I type "C-g"
(setq visible-bell t)

;; If the window is too narrow, make the mode-line compact
(setq mode-line-compact 'long)

;; Make links clickable (https://gnu.org) "C-c RET"
(global-goto-address-mode)

;; Change the mode-line a little bit
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

;; org-mode
(use-package org
  :hook (org-mode . auto-fill-mode)
  :custom
  (org-edit-src-content-indentation 0)
  (org-startup-indented t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (emacs-lisp . t)))
)
(use-package org-sticky-header
  :after (org)
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-always-show-header nil)
  (org-sticky-header-full-path 'reversed))

;;; Other configs
(electric-pair-mode t)
(setq-default truncate-lines t)
(require 'gdb-mi)
(gdb-many-windows 1)
(require 'server)
(unless (server-running-p)
  (server-start))
(put 'narrow-to-region 'disabled nil) ;; "C-x n n" / "C-x n w"
(setq set-mark-command-repeat-pop t) ;; "C-u C-SPC"
(put 'scroll-left 'disabled nil) ;; "C-x <"

;;;;; Functions ;;;;;

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

(defun insert-random-number-with-n-digits (*n) ;; (random t) to increase randomness
  "Insert *n random digits. *n default to 5.
Call `universal-argument' before for different count."
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount)))))
  )

(defun insert-random-number (n)
  "Insert a random number between 0 and `universal-argument'.
Default is 1000."
  (interactive "P")
  (message "%s" n)
  (insert (number-to-string (random (if (numberp n) (identity n) 1000))))
  )

(defun ryanmarcus/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

;;;;; Keybindings ;;;;;

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

;; The code above creates 'my-mode-map', which will override major modes keymaps
(define-key my-mode-map (kbd "C-c c") 'comment-or-uncomment-region)
(define-key my-mode-map (kbd "C-c TAB") 'align-regexp)
(define-key my-mode-map (kbd "<up>") 'windmove-up)
(define-key my-mode-map (kbd "<down>") 'windmove-down)
(define-key my-mode-map (kbd "<left>") 'windmove-left)
(define-key my-mode-map (kbd "<right>") 'windmove-right)

(global-set-key (kbd "C-c DEL") 'delete-trailing-whitespace)
(global-set-key (kbd "M-q") 'fill-region)
(global-set-key [C-backspace] 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "C-c n") 'duplicate-line)

;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; use-package is shipped with Emacs 29, this will be no longer necessary soon
;; Install use-package automatically
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;;; Generic packages ;;;;;

;; Set theme
(use-package dracula-theme
  :config
  ;; Dracula is horrific in a limited color space. Changing theme when in a
  ;; non-graphical environment
  (if (or (display-graphic-p) (daemonp))
    (load-theme 'dracula t)
  (disable-theme 'dracula)
  (load-theme 'manoj-dark)
  ))

;; Support fonts with ligatures
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Set background color to strings that match color names
(use-package rainbow-mode
  :hook (html-mode css-mode scss-mode js-mode))

;; Custom scroll bar
(use-package yascroll
  :init
  (scroll-bar-mode -1)
  :custom
  (global-yascroll-bar-mode t))

;; Custom mode-line
(use-package mood-line
  ;; Enable mood-line
  :config
  (mood-line-mode)
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

;; Better window switching
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Save minibuffer history. Useful for vertico
(use-package savehist
  :init (savehist-mode))

;; Minibuffer completion UI
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t))

;; `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Text completion
(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-prefix 2)             ;; Minimum length for auto completion.
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-at-boundary nil)      ;; Never quit at completion boundary
  (corfu-quit-no-match t)           ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :config
  (unbind-key "RET" corfu-map)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)

  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :after (corfu)
  :config
  (unless (or (display-graphic-p) (daemonp))
    (corfu-terminal-mode +1)))

;; Snippets support
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

;; Spellchecking (install Aspell and the dictionaries on your system)
;; Spell check a single word or region: M-$
;; Change the language of a single buffer: M-x ispell-change-dictionary
(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "brasileiro")
  (ispell-dictionary-alist (ispell-find-aspell-dictionaries))
  )
(use-package flyspell
  :hook
  (org-mode . flyspell-mode)
  (text-mode . (lambda () ;; Don't start flyspell in the scratch buffer
                 (unless (string= (buffer-name) "*scratch*") (flyspell-mode))
                 ))
  )

;; Multiple cursors
(use-package multiple-cursors
  ; (C-') To hide all lines with no cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this-symbol)
         ("C-S-k"         . mc/skip-to-next-like-this)
         ("C-<"           . mc/mark-previous-like-this-symbol)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-c C->"       . mc/mark-all-like-this-dwim) ;; Change HTML tag
         ("C-M-<"         . mc/unmark-next-like-this)
         ("C-M->"         . mc/unmark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
              ("<return>" . nil)))

;; Cool font icons
(use-package all-the-icons
  :when (or (display-graphic-p) (daemonp)))
(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; New startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "bookmark")))
  :custom
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
                         "Make dashboard open on emacsclient frames")
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     ))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  )

;; Move a selection up and down the lines
(use-package drag-stuff
  :bind (("M-S-<up>" . drag-stuff-up)
         ("M-S-<down>" . drag-stuff-down)))

;; Press f8 to open the current directory's tree
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-theme (if (or (display-graphic-p) (daemonp)) 'icons 'arrow)))

;; Pop-up buffers
(use-package popper
  :bind (("C-S-j"   . popper-toggle-latest)
         ;; ("M-`"   . popper-cycle)
         ("C-M-j" . popper-toggle-type)
         )
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          term-mode))
  (popper-mode +1)
  (popper-echo-mode +1) ; For echo area hints
  :hook (term-mode . (lambda ()
                       (let ((buf (current-buffer)))
                         (bury-buffer)
                         (switch-to-buffer-other-window buf)
                         )))
  )

;; which-key
(use-package which-key
  :custom
  (which-key-idle-delay 2.0)
  :config
  (which-key-mode)
  )

;;;;; IDE-like features ;;;;

;; Front-end for git
(use-package magit)

;; Display which lines were changed since the last commit.
(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
  :custom
  (git-gutter:update-interval 2))

;; Language server protocol support (smart text completion)
;; Eglot will be part of emacs 29.
(use-package eglot
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (java-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown 1)
  :bind ("C-<f2>" . eglot-rename))
; Eglot keybindings:
; "M-." goto symbol definition
; "M-," go back (after "M-.")
; "M-?" find references to a symbol
; "C-h-." display symbol help
; Useful funcions
; "imenu" search definition IN THE CURRENT FILE

;; Underline errors, warnings and suggestions as you type
(use-package flymake
  :config
  (flymake-mode 1))

;; "C-<return>" to fold a code block
(use-package origami
  :hook (prog-mode . origami-mode)
  :bind ("C-<return>" . origami-forward-toggle-node))

;; Documentation at point
(use-package eldoc-box
  :after (eglot)
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  :custom
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-max-pixel-width 500))

;; Modular debugger
(use-package realgud)
;; "M-x realgud:DEBUGGERNAME" to start debugging

;; Install clangd to enable lsp features for C/C++
(use-package cc-mode
  :config
  ;; Change indentation style because the default is GNU and I don't like it
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (add-to-list 'c-default-style '(c++-mode . "linux"))
  )

(add-hook 'c-mode-hook
          (lambda ()
	    (unless (or (file-exists-p "makefile")
		        (file-exists-p "Makefile"))
              (setq-local compile-command
		          (concat "gcc -g -Wall -lm "
			          (if buffer-file-name
			              (shell-quote-argument buffer-file-name)))))))
(add-hook 'c-mode-hook (lambda () (setq-local c-basic-offset 4)))
(add-hook 'c++-mode-hook (lambda () (setq-local c-basic-offset 4)))

(use-package cmake-mode)
(use-package eldoc-cmake
  :hook (cmake-mode . eldoc-cmake-enable))

;; Auto-format code with clang-format when saving the file
(use-package clang-format+
  :hook
  (c-mode . clang-format+-mode)
  (c++-mode . clang-format+-mode))

;; Eclipse JDT Language Server is hard to work with. eglot-java automates a lot of things
(use-package eglot-java
  :hook (java-mode . eglot-java-mode))

(use-package groovy-mode)

;; Install hls to enable lsp features for Haskell
(use-package haskell-mode
  :hook (haskell-mode . (lambda ()
                          (push '("\\" . ?λ) prettify-symbols-alist)))
  (haskell-mode . interactive-haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'prettify-symbols-mode 1)
  :custom
  (haskell-interactive-popup-errors nil))
;; "C-c C-l" Start Haskell REPL

(use-package nix-mode)
