;; Emacs 28

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p (concat user-emacs-directory "custom.el"))
  (load custom-file))

;;;;; Configs that don't depend on any package ;;;;;

(defvar backup-directory (concat user-emacs-directory "backups") "Directory where all backup files will be stored.")
(unless (file-directory-p backup-directory)
  (make-directory backup-directory))
(setq backup-directory-alist `((".*" \, backup-directory)))

;; Tool-bar is useless
(tool-bar-mode -1)

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

;; If the window is too narrow, make the mode-line compact
(setq mode-line-compact 'long)

;; Please, don't make noise when I type "C-g"
(setq visible-bell t)

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

;;; Other configs
(electric-pair-mode t)
(setq-default truncate-lines t)
(require 'gdb-mi)
(gdb-many-windows 1)
(require 'server)
(unless (server-running-p)
  (server-start))

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

(defun insert-random-number (*n) ;; (random t) to increase randomness
  "Insert *n random digits. *n default to 5.
Call `universal-argument' before for different count."
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount)))))
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
(define-key my-mode-map (kbd "M-<up>") 'windmove-up)
(define-key my-mode-map (kbd "M-<down>") 'windmove-down)
(define-key my-mode-map (kbd "M-<left>") 'windmove-left)
(define-key my-mode-map (kbd "M-<right>") 'windmove-right)

(global-set-key (kbd "C-c <backspace>") 'delete-trailing-whitespace)
(global-set-key [C-backspace] 'ryanmarcus/backward-kill-word)
(global-set-key (kbd "C-c DEL") 'delete-trailing-whitespace)
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

;; Custom scroll bar
(use-package yascroll
  :init
  (scroll-bar-mode -1)
  :custom
  (global-yascroll-bar-mode t))

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
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil "Make company case-sensitive")
  (company-idle-delay 0 "Delay to show avaliable completions")
  (company-minimum-prefix-length 1)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-<tab>" . company-complete-common)
              ("<return>" . nil)
              ("RET" . nil)
              )
  :config
  (use-package company-statistics
    :config (company-statistics-mode)))

;; Better UI for company
(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode))

;; Snippets support
(use-package yasnippet
  :after (company)
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
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
  :when (or (display-graphic-p) (daemonp))
  :custom
  (all-the-icons-dired-mode t))

;; New startup screen
(use-package dashboard
  :after (all-the-icons)
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
  :bind (("C-S-<up>" . drag-stuff-up)
         ("C-S-<down>" . drag-stuff-down)))

;; Press f8 to open the current directory's tree
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-theme (if (or (display-graphic-p) (daemonp)) 'icons 'arrow)))

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
(use-package lsp-mode
  :after (haskell-mode lsp-haskell)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode c++-mode haskell-mode) . lsp)))

;; Show lsp messages in sideline
(use-package lsp-ui
  :after (lsp-mode)
  :commands lsp-ui-mode)

;; Underline errors, warnings and suggestions as you type
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; "C-<return>" to fold a code block
(use-package origami
  :hook (prog-mode . origami-mode)
  :bind ("C-<return>" . origami-forward-toggle-node))

;; Change indentation style because the default is GNU and I don't like it
(use-package cc-mode
  :config
  (add-to-list 'c-default-style '(c-mode . "linux"))
  (add-to-list 'c-default-style '(c++-mode . "linux"))
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook
          (lambda ()
	    (unless (or (file-exists-p "makefile")
		        (file-exists-p "Makefile"))
              (setq-local compile-command
		          (concat "gcc -g -Wall -lm "
			          (if buffer-file-name
			              (shell-quote-argument buffer-file-name)))))))

;; Auto-format code with clang-format when saving the file
(use-package clang-format+
  :hook (c-mode . clang-format+-mode))

(use-package haskell-mode
  :hook ((haskell-mode . (lambda ()
                           (push '("\\" . ?λ) prettify-symbols-alist)))
         )
  :config
  (add-hook 'haskell-mode-hook 'prettify-symbols-mode 1))

(use-package lsp-haskell)

;; DAP support (debugger) !(it isn't working at the moment)
;; https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/#debugging
;; https://code.visualstudio.com/docs/cpp/launch-json-reference
;; You maybe have to compile this by hand: https://github.com/llvm/llvm-project/tree/main/lldb/tools/lldb-vscode
;; The above is probably false
(use-package dap-mode
  :config
  (require 'dap-cpptools)

  (dap-register-debug-template
  "c++::Run a.out"
  (list :name "c++::Run a.out"
        :type "cppdbg"
        :request "launch"
        :MIMode "gdb"
;        :miDebuggerPath "/home/thiago/.nix-profile/bin/gdb"
        :program "${workspaceFolder}/a.out"
        :cwd "${workspaceFolder}"))
  )
;; Create a launch.json for each project to use a custom debug configutarion
;; for it.
