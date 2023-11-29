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
    signal-desktop
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
    shellInit = ''
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
  programs.bash.enable = true;
  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
      arguments = [ "-c" "-a ${pkgs.emacs}/bin/emacs" ];
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
      shell.program = "${pkgs.fish}/bin/fish";
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
