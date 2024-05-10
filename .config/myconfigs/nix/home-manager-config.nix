{ pkgs, ... }:
let
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of
      latexmk; # org-latex-export-to-pdf
  });
  session-quit = (pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "ThwyIgo";
    repo = "session-quit";
    rev = "6a05ffa49d3099eee2b696559010490b4b51e500";
    hash = "sha256-j5qYQeMnixqCEXdZexjbLErj9ewaBm2KblQeQxOepwM=";
  }) {}).session-quit;
in
{
  home.stateVersion = "22.05";
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    # CL
    mate.mate-polkit

    # Libs
    qt6Packages.qtstyleplugin-kvantum
    aspell
    aspellDicts.pt_BR
    aspellDicts.en
    tex

    # GUI
    session-quit
    keepassxc
    virt-manager
    tdesktop # Telegram
    discord
    spotify
    tenacity # Audacity
    zathura
    prismlauncher

    # Programming
    nixd
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
    fira-code
    emacs-all-the-icons-fonts
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

  # automount
  services.udiskie.enable = true;

  # programs.ssh = {
  #   enable = true;
  #   addKeysToAgent = "30m";
  # };
  # services.ssh-agent.enable = true;

  fonts.fontconfig.enable = true;

  home.pointerCursor = {
    package = pkgs.nordzy-cursor-theme;
    name = "Nordzy-cursors-white";
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

  home.sessionVariables = {
    ASPELL_CONF = "data-dir \${HOME}/.nix-profile/lib/aspell";
  };
}
