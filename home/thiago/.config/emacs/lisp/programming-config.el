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

  :hook ((eglot-connect . eldoc-mode)
         (rust-mode . eglot-ensure)
         (nix-ts-mode . eglot-ensure)
         (lua-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (csharp-ts-mode . eglot-ensure)))
;; Eglot keybindings:
;; "M-." goto symbol definition
;; "M-," go back (after "M-.")
;; "M-?" find references to a symbol
;; "C-h-." display symbol help
;; "M-g i" or "imenu" search definition IN THE CURRENT FILE

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
  :hook (rust-ts-mode . treesit-fold-mode)
  :init
  (setq rust-mode-treesitter-derive t))

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

;; Support for Haskell
;; (use-package haskell-ts-mode
;;   :vc (:url "https://github.com/dschrempf/haskell-ts-mode" :rev :newest)
;;   :custom
;;   ;; Optional; both differ from the default.
;;   (haskell-ts-font-lock-level 3)
;;   (haskell-ts-prettify-symbols t))

(use-package csharp-mode
  :commands csharp-ts-mode
  :mode ("\\.cs\\'" . csharp-ts-mode)
  :hook (csharp-ts-mode . treesit-fold-mode))

(provide 'programming-config)
;;; programming-config.el ends here
