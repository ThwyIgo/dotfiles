{ config, lib, pkgs, minimal-emacs-src, ... }:
{
  home.stateVersion = "25.05";

  home.packages = with pkgs; [
    # GUI
    keepassxc
    virt-manager
    winapps
    spotify
    zathura
    thunderbird
    obs-studio
    libreoffice-stable
    ## Dicionários para LibreOffice
    (hunspell.withDicts (dicts: with dicts;
      [pt_BR en_US]))
    slack

    # Programming
    bruno
    dbgate
    dotnetCorePackages.sdk_10_0
    antigravity-ide
    python3

    # Fonts
    fira-code
  ] ++ [
    # LSP
    nixd
    package-version-server
    omnisharp-roslyn
    lua-language-server
    ty
    ruff
    angular-language-server
    clang-tools
    neocmakelsp
    nginx-language-server
    bash-language-server
    shellcheck

    # Debuggers
    netcoredbg
    lldb
    gdb
  ];
  programs.librewolf = {
    enable = true;
    languagePacks = [ "pt-BR" "en-US" "de" ];
    settings = {
      "widget.gtk.libadwaita-colors.enabled" = false;
      "identity.fxaccounts.enabled" = true;
      "privacy.clearOnShutdown.history" = false;
      "privacy.clearOnShutdown.downloads" = false;
    };
  };
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
  };
  programs.fish = {
    enable = true;
    shellInit = ''
      set fish_color_user 5cf brblue
      set fish_color_cwd brgreen
      set fish_greeting
    '';
  };
  programs.bash.enable = true;
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;
  };
  home.shellAliases = {
    gitlog = "git log --decorate --oneline --graph";
    dfgit = "git --git-dir=$HOME/.dotfiles --work-tree=/";
    dfgitlog = "dfgit log --decorate --oneline --graph";
  };
  services.kdeconnect.enable = true;
  services.ollama = {
    enable = true;
    acceleration = "cuda";
    package = pkgs.ollama-cuda;
  };
  programs.java.enable = true;

  programs.zed-editor = {
    enable = true;
    extraPackages = with pkgs; [];
  };
  programs.zed-editor-extensions = {
    enable = true;
    packages = with pkgs.zed-extensions; [
      dockerfile docker-compose java sql lua csharp log nix nginx org neocmake
      toml xml netcoredbg angular
    ];
  };
  home.file = {
    ".local/share/zed/debug_adapters/CodeLLDB".source = "${pkgs.vscode-extensions.vadimcn.vscode-lldb}/share/vscode/extensions/vadimcn.vscode-lldb/";
    ".local/share/zed/nix/jdtls/".source = toString pkgs.jdt-language-server;
    ".local/share/zed/nix/lombok/lombok.jar".source = "${pkgs.lombok.out}/share/java/lombok.jar";
    ".local/share/zed/nix/com.microsoft.java.debug.plugin.jar".source = "${pkgs.vscode-extensions.vscjava.vscode-java-debug}/share/vscode/extensions/vscjava.vscode-java-debug/server/com.microsoft.java.debug.plugin-0.53.2.jar";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      treesit-grammars.with-all-grammars
    ];
  };
  services.emacs = {
    enable = true;
    defaultEditor = true;
    client = {
      enable = true;
      arguments = [ "-c" "-a" "emacs" ];
    };
  };

  xdg.enable = true;
  xdg.configFile = lib.pipe (builtins.readDir minimal-emacs-src) [
    (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".el" name))
    (lib.mapAttrs' (name: _: lib.nameValuePair "emacs/${name}" {
      source = "${minimal-emacs-src}/${name}";
    }))
  ];

  programs.git = {
    enable = true;
    settings = {
      user.email = "thiago.rocha@triliton.com.br";
      user.name = "thiagorocha6";
    };
    ignores = [
      "*~"
      ".fuse_hidden*"
      ".directory"
      ".Trash-*"
      ".nfs*"
      ".dir-locals.el"
    ];
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    settings = {
      "bitbucket.org" = {
        IdentityFile = "~/.ssh/bitbucket-triliton_rsa";
      };
      "github.com" = {
        IdentityFile = [ "~/.ssh/gh-ThwyIgo" "~/.ssh/gh-alan-petrobras" ];
      };
    };
  };

  xdg.autostart = {
    enable = true;
    readOnly = true;
    entries = [
      (pkgs.makeDesktopItem {
        destination = "";
        name = "Slack";
        desktopName = "Slack";
        startupWMClass = "Slack";
        exec = "${pkgs.slack}/bin/slack -su %U";
        icon = "slack";
        startupNotify = true;
        mimeTypes = [ "x-scheme-handler/slack" ];
      } + "/Slack.desktop")
    ];
  };

  services.ssh-agent.enable = true;

  fonts.fontconfig.enable = true;

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
  # See https://wiki.nixos.org/wiki/Virt-manager#Wayland
  home.pointerCursor = {
    enable = true;
    gtk.enable = true;
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };
}
