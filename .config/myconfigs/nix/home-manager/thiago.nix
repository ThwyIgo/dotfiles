{ config, pkgs, stylix, ... }:
let
  session-quit = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "ThwyIgo";
    repo = "session-quit";
    rev = "c22e4b344fb9378c37190556617812aa04268789";
    hash = "sha256-/dGXzVdX8O4DfqIbW0L6+dsFsXAA7VFFoOJioSSRoAo=";
  }) {};
  dracula-theme-qt = pkgs.callPackage ../pkgs/dracula-theme-qt.nix {};
in
{
  home.stateVersion = "22.05";
  #nixpkgs.config.allowUnfree = true;
  imports = [ stylix ];

  home.packages = with pkgs; [
    # CL
    mate.mate-polkit
    alsa-utils

    # Libs
    (aspellWithDicts (dicts: with dicts; [ en pt_BR ]))

    # GUI
    session-quit
    keepassxc
    virt-manager
    tdesktop # Telegram
    discord
    stremio
    spotify
    zathura
    prismlauncher
    bottles
    musescore

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
    picom
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
  programs.alacritty = {
    enable = true;
    settings = {
      window.opacity = 0.9;
      terminal.shell.program = "${pkgs.fish}/bin/fish";
    };
  };
  programs.rofi = {
    enable = true;
    font = "Ubuntu Normal Medium 12";
    theme = "dmenu";
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  # automount
  services.udiskie.enable = true;

  programs.git = {
    enable = true;
    userEmail = "thiagopachecorocha@hotmail.com";
    userName = "ThwyIgo";
  };

  programs.ssh = {
    enable = true;
    addKeysToAgent = "30m";
    matchBlocks = {
      "*".identityFile = [ "~/.ssh/id_rsa" "~/.ssh/ufu" ];
    };
  };
  services.ssh-agent.enable = true;

  fonts.fontconfig.enable = true;

  qt = {
    enable = true;
    platformTheme.name = "qtct";
  };

  xdg.configFile = {
    "qt5ct/qt5ct.conf".text = ''
      [Appearance]
      color_scheme_path=${dracula-theme-qt}/share/qt5ct/colors/Dracula.conf
      custom_palette=true
      style=Breeze
    '';
  };

  stylix = {
    enable = true;
    image = pkgs.fetchurl {
      url = "https://www.pixelstalk.net/wp-content/uploads/2016/11/Iceland-mountains-hd-wallpapers.jpg";
      sha256 = "sha256-M+9/GE0POxFOJ0ydH8SOpbOIeuqUvjayq4JozRHzAzI=";
    };
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/dracula.yaml";
    opacity.terminal = 0.9;
    fonts = {
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
      sansSerif = {
        package = pkgs.ubuntu_font_family;
        name = "Ubuntu";
      };
      monospace = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans Mono";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
    cursor = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors-white";
      size = 24;
    };
    targets = builtins.listToAttrs
      (map (a: {name = a; value = {enable = false;};}) [
                  "rofi" "emacs" "vscode"
                ]);
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/.local/share/wallpapers";
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
