{ pkgs, ... }:
let
  vimix-cursors = pkgs.callPackage ./vimix-cursors.nix {};
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of
      latexmk; # org-latex-export-to-pdf
  });
in
{
  home.stateVersion = "22.05";
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    # Libs
    libsForQt5.qtstyleplugin-kvantum
    aspell
    aspellDicts.pt_BR
    aspellDicts.en
    tex

    # GUI
    keepassxc
    virt-manager
    tdesktop # Telegram
    discord
    spotify
    tenacity # Audacity
    blender
    zathura

    # Programming
    rnix-lsp
    gdb
    clang-tools # Clangd
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      xmobar
      xmonad
      xmonad-contrib
    ]))
    haskell-language-server
    rstudio

    # Window Manager stuff
    haskellPackages.xmobar
    picom-jonaburg
    networkmanagerapplet
    trayer
    feh
    playerctl

    # Fonts
    monocraft
  ];
  programs.fish = {
    enable = true;
    functions = {
      fish_right_prompt = "echo [(date '+%H:%M')]";
    };
    interactiveShellInit = ''
        set fish_color_user 5cf brblue
        set fish_color_cwd brgreen
        set fish_greeting
      '';
    shellAliases = {
      gitlog = "git log --decorate --oneline --graph";
      dfgit = "git --git-dir=$HOME/.dotfiles --work-tree=$HOME";
      dfgitlog = "dfgit log --decorate --oneline --graph";
    };
  };
  programs.bash = {
    enable = true;
  };
  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    defaultEditor = true;
  };
  xdg.desktopEntries.emacsclient = {
    type = "Application";
    name = "Emacs Client";
    genericName = "Text Editor";
    comment = "Edit text";
    categories = [ "Development" "TextEditor" ];
    exec = "sh -c \"if [ -n \\\"\\$*\\\" ]; then exec emacsclient --alternate-editor= --display=\\\"\\$DISPLAY\\\" \\\"\\$@\\\"; else exec emacsclient --alternate-editor= --create-frame; fi\" placeholder %F";
    icon = "emacs";
    settings = {
      Keywords = "Text;Editor;";
      StartupWMClass = "Emacs";
    };
    mimeType = [ "text/english" "text/plain" "text/x-makefile" "text/x-c++hdr" "text/x-c++src" "text/x-chdr" "text/x-csrc" "text/x-java" "text/x-moc" "text/x-pascal" "text/x-tcl" "text/x-tex" "application/x-shellscript" "text/x-c" "text/x-c++" ];
    terminal = false;
    actions = {
      "new-window" = {
        name = "Nova janela";
        exec = "emacsclient --alternate-editor= --create-frame %F";
      };
      "new-instance" = {
        name = "Nova instância";
        exec = "emacs %F";
      };
    };
  };
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
  };
  programs.alacritty = {
    enable = true;
    settings = {
      window.opacity = 0.9;
      shell.program = "fish";
    };
  };
  programs.rofi = {
    enable = true;
    font = "Ubuntu Normal Medium 12";
    theme = "dmenu";
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  services.kdeconnect.indicator = true;

  home.pointerCursor = {
    package = vimix-cursors;
    name = "Vimix-white-cursors";
    size = 24;

    x11.enable = true;
    gtk.enable = true;
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-Grey-Dark";
    };
    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela-dark";
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "kvantum";
  };

  home.file."kvantum.kvconfig" = {
    target = ".config/Kvantum/kvantum.kvconfig";
    text = "[General]\ntheme=KvGnomeDark";
  };

  # 漢語
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [ fcitx5-chinese-addons ];
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
