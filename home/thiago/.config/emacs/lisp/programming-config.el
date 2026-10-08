;;; programming-config.el --- Programming configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package magit)

;; Support for Git files (.gitconfig, .gitignore, .gitattributes...)
(use-package git-modes
  :commands (gitattributes-mode
             gitconfig-mode
             gitignore-mode)
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)

         ("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/etc/gitconfig\\'" . gitconfig-mode)

         ("/\\.gitattributes\\'" . gitattributes-mode)
         ("/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'" . gitattributes-mode)))

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-events-buffer-config '(:size 0 :format short))
  ;; Disable automatic code action indicators to reduce background polling
  (setq eglot-code-action-indications nil)
  (put 'eglot-flymake-backend 'flymake-always-safe t)

  :hook ((eglot-connect . eldoc-mode)
         (rust-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (lua-ts-mode . eglot-ensure)
         (csharp-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)))
;; Eglot keybindings:
;; "M-." goto symbol definition
;; "M-," go back (after "M-.")
;; "M-?" find references to a symbol
;; "C-h-." display symbol help
;; "M-g i" or "imenu" search definition IN THE CURRENT FILE

(use-package dape
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  ;; (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  ;; (dape-buffer-window-arrangement 'gud)
  ;; (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)

  :config
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook #'kill-buffer)
  )
;; Configure dape by customizing the variable dape-configs

(put 'dape-configs 'safe-local-variable #'listp)

;; Left and right side windows occupy full frame height
;; When nil:
;; +------------------------------------+
;; |            TOP WINDOW              |  <-
;; +---------+----------------+---------+
;; |  LEFT   |  BUFFER PRINC. |  RIGHT  |
;; +---------+----------------+---------+
;; |           BOTTOM WINDOW            |  <-
;; +------------------------------------+
;; When not nil:
;; +----+--------------------------+----+
;; |    |        TOP WINDOW        |    |
;; | L  +--------------------------+  R |
;; | E  |                          |  I |
;; | F  |     BUFFER PRINCIPAL     |  G |  <-
;; | T  |                          |  H |
;; |    +--------------------------+  T |
;; |    |      BOTTOM WINDOW       |    |
;; +----+--------------------------+----+
(use-package emacs
  :custom
  (window-sides-vertical t))

(add-hook 'c-ts-mode-hook #'treesit-fold-mode)
(add-hook 'c++-ts-mode-hook #'treesit-fold-mode)
(add-hook 'php-ts-mode-hook #'treesit-fold-mode)
(add-hook 'css-ts-mode-hook #'treesit-fold-mode)
(add-hook 'html-ts-mode-hook #'treesit-fold-mode)
(add-hook 'bash-ts-mode-hook #'treesit-fold-mode)

;; Configure built-in sgml-mode to automatically enable
;; `sgml-electric-tag-pair-mode' in `html-mode' and `mhtml-mode', providing
;; automatic insertion of matching closing tags.
(use-package sgml-mode
  :ensure nil
  :commands (sgml-mode sgml-electric-tag-pair-mode)
  :hook ((html-mode mhtml-mode) . sgml-electric-tag-pair-mode))

;; Support for YAML files.
(use-package yaml-ts-mode
  :commands yaml-ts-mode
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . treesit-fold-mode))

;; Support for Dockerfile files.
(use-package dockerfile-ts-mode
  :commands dockerfile-ts-mode
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :hook (dockerfile-ts-mode . treesit-fold-mode))

;; Support for Gnuplot files
;; (use-package gnuplot
;;   :commands gnuplot-mode
;;   :mode ("\\.gp\\'" . gnuplot-mode))

;; Support for *.lua files.
(use-package lua-ts-mode
  :commands lua-ts-mode
  :mode ("\\.lua\\'" . lua-ts-mode)
  :hook (lua-ts-mode . treesit-fold-mode))

;; Jinja2 template support for files commonly used in configuration management
;; systems and web frameworks. This mode enables syntax highlighting and basic
;; editing facilities for templates written using the Jinja2 templating
;; language.
;; (use-package jinja2-mode
;;   :commands jinja2-mode
;;   :mode ("\\.j2\\'" . jinja2-mode))

;; Support for Go
(use-package go-ts-mode
  :commands go-ts-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . treesit-fold-mode))

;; Support for Rust
(use-package rust-mode
  :commands rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook
  (rust-ts-mode . treesit-fold-mode)
  (rust-ts-mode . prettify-symbols-mode)
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  (rust-format-on-save t))

;; Major mode for editing crontab files
(use-package crontab-mode
  :commands crontab-mode
  :mode ("/crontab\\(\\.X*[[:alnum:]]+\\)?\\'"  . crontab-mode))

;; Major mode for editing Nginx configuration files
(use-package nginx-mode
  :commands nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

;; Major mode for HashiCorp Configuration Language (HCL) files
;; (use-package hcl-mode
;;   :commands hcl-mode
;;   :mode ("\\.hcl\\'" . hcl-mode))

;; Major mode for Nix expression language files
(use-package nix-ts-mode
  :commands nix-ts-mode
  :mode ("\\.nix\\'". nix-ts-mode)
  :hook (nix-ts-mode . treesit-fold-mode))

;; Major mode for editing Fish shell scripts
(use-package fish-mode
  :commands fish-mode
  :mode ("\\.fish\\'" . fish-mode))

;; Vim configuration file support. This mode provides syntax highlighting and
;; editing support for various Vim configuration files, including vimrc, gvimrc,
;; local overrides, and project-specific configuration files.
(use-package vimrc-mode
  :commands vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; Support for Jenkinsfile files
(use-package jenkinsfile-mode
  :commands jenkinsfile-mode
  :mode ("Jenkinsfile\\'" . jenkinsfile-mode))

(use-package csharp-mode
  :commands csharp-ts-mode
  :mode ("\\.cs\\'" . csharp-ts-mode)
  :hook (csharp-ts-mode . treesit-fold-mode))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . (lambda ()
                          (setq prettify-symbols-alist
                                '(("\\"        . ?λ)
                                  ("`elem`"    . ?∈)
                                  ("`notElem`" . ?∉)
                                  ("forall"    . ?∀)))))
  (haskell-mode . interactive-haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'prettify-symbols-mode 1)
  :custom
  (haskell-interactive-popup-errors nil))
;; "C-c C-l" Start Haskell REPL

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
         ("r" . project-run))
  :config
  (defcustom project-run-commands nil
    "Alist de comandos para executar no projeto.
Cada item deve ser no formato:
  (NOME . COMANDO)
ou
  (NOME COMANDO :env (...))
ou
  (NOME :command COMANDO :env (...))

Onde :env pode ser uma lista de strings (\"VAR=VAL\"), uma plist (:VAR \"VAL\")
ou uma alist ((\"VAR\" . \"VAL\")). As variáveis definidas em :env têm precedência
sobre `compilation-environment'."
    :type '(alist :key-type string :value-type sexp)
    :group 'project)

  (put 'project-run-commands 'safe-local-variable #'listp)

  (defun project-run--normalize-env (env)
    "Normaliza ENV para uma lista de strings no formato \"VAR=VAL\"."
    (cond
     ((null env) nil)
     ;; alist: (("VAR" . "VAL") ...)
     ((and (consp env) (consp (car env)))
      (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) env))
     ;; plist: (:VAR "VAL" ...)
     ((and (consp env) (keywordp (car env)))
      (let (res)
        (while env
          (let ((k (car env))
                (v (cadr env)))
            (setq env (cddr env))
            (push (format "%s=%s" (substring (symbol-name k) 1) v) res)))
        (nreverse res)))
     ;; lista de strings: ("VAR=VAL" ...)
     ((and (listp env) (stringp (car env)))
      env)
     (t nil)))

  (defun project-run--parse-task (spec)
    "Retorna um cons cell (COMANDO . LISTA-ENV) a partir de SPEC."
    (cond
     ((stringp spec)
      (cons spec nil))
     ((and (listp spec) (keywordp (car spec)))
      (cons (plist-get spec :command)
            (project-run--normalize-env (plist-get spec :env))))
     ((and (consp spec) (stringp (car spec)))
      (let* ((cmd (car spec))
             (rest (cdr spec))
             (env (if (keywordp (car rest))
                      (plist-get rest :env)
                    (car rest))))
        (cons cmd (project-run--normalize-env env))))
     (t
      (cons nil nil))))

  (defun project-run (&optional edit-cmd)
    "Pergunta qual tarefa de execução de `project-run-commands' executar.
Executa o comando no diretório raiz do projeto atual via `compile'.
Variáveis definidas no :env da tarefa são injetadas em `compilation-environment'.
Com prefix argument EDIT-CMD (\\[universal-argument]), permite editar o comando
antes de executá-lo."
    (interactive "P")
    (let* ((pr (project-current t))
           (root (project-root pr))
           (default-directory root)
           (compilation-buffer-name-function
            (or (bound-and-true-p project-compilation-buffer-name-function)
                compilation-buffer-name-function)))
      (unless project-run-commands
        (user-error "Nenhuma tarefa configurada em `project-run-commands' para este projeto"))
      (let* ((choice (if (= (length project-run-commands) 1)
                         (caar project-run-commands)
                       (completing-read "Executar tarefa: "
                                        (mapcar #'car project-run-commands)
                                        nil t)))
             (task-spec (alist-get choice project-run-commands nil nil #'equal))
             (parsed (project-run--parse-task task-spec))
             (cmd (car parsed))
             (task-env (cdr parsed)))
        (unless cmd
          (user-error "Comando não encontrado para a tarefa: %s" choice))
        (when edit-cmd
          (setq cmd (read-shell-command "Comando: " cmd)))
        (let ((compilation-environment (append task-env (bound-and-true-p compilation-environment))))
          (compile cmd)))))

  (add-to-list 'project-switch-commands '(project-run "Run task" "r")))

(provide 'programming-config)
;;; programming-config.el ends here

