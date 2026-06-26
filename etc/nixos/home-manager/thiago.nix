{ config, pkgs, ... }:
{
  home.stateVersion = "25.05";

  home.packages = with pkgs; [
    # GUI
    mission-center
    keepassxc
    virt-manager
    winapps
    spotify
    zathura
    thunderbird
    pix
    obs-studio
    libreoffice-still
    ## Dicionários para LibreOffice
    (hunspell.withDicts (dicts: with dicts;
      [pt_BR en_US]))
    slack

    # Programming
    bruno
    dbgate
    jetbrains.idea-oss
    dotnetCorePackages.sdk_10_0

    # Fonts
    fira-code
  ];
  programs.librewolf = {
    enable = true;
    languagePacks = [ "pt-BR" "en-US" ];
    settings = {
      "widget.gtk.libadwaita-colors.enabled" = false;
      "identity.fxaccounts.enabled" = true;
      "privacy.clearOnShutdown.history" = false;
      "privacy.clearOnShutdown.downloads" = false;
    };
  };
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
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
  services.ollama = {
    enable = true;
    acceleration = "cuda";
    package = pkgs.ollama-cuda;
  };
  programs.java.enable = true;

  programs.zed-editor = {
    enable = true;
    extraPackages = with pkgs; [
      nixd
      package-version-server
      omnisharp-roslyn
      lua-language-server
      ty
      ruff
      clang-tools
      lldb
      gdb
      neocmakelsp
      bash-language-server
      shellcheck
      netcoredbg
      angular-language-server
    ];
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

  programs.vscodium.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

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
        IdentityFile = "~/.ssh/gh-ThwyIgo";
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

  home.sessionVariables = {
    VISUAL = pkgs.zed-editor.meta.mainProgram + " -w";
    EDITOR = pkgs.micro.meta.mainProgram;
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
  # See https://wiki.nixos.org/wiki/Virt-manager#Wayland
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
  };
}
