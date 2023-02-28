# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-335510bd-4e31-443b-a111-585046c88f80".device = "/dev/disk/by-uuid/335510bd-4e31-443b-a111-585046c88f80";
  boot.initrd.luks.devices."luks-335510bd-4e31-443b-a111-585046c88f80".keyFile = "/crypto_keyfile.bin";

  system.autoUpgrade.enable = true;
  system.autoUpgrade.operation = "boot";

  networking.hostName = "PeaceNixArch"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    persistent = true;
    options = "--delete-older-than 5d";
  };

  nix.extraOptions = "plugin-files = ${pkgs.nix-doc}/lib/libnix_doc_plugin.so";

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "pt_BR.utf8";
  i18n.supportedLocales = [ "pt_BR.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the Cinnamon Desktop Environment.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.cinnamon.enable = true;
  environment.cinnamon.excludePackages = with pkgs; [
    xed-editor
    hexchat
    gnome.geary
    xplayer
  ];

  # Enable Xmonad window manager
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  # Auto-login
  # services.xserver.displayManager.defaultSession = "cinnamon";
  # services.xserver.displayManager.autoLogin.enable = true;
  # services.xserver.displayManager.autoLogin.user = "thiago";

  # Configure keymap in X11
  services.xserver = {
    layout = "br";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "br-abnt2";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  services.avahi.enable = true; # Auto-detect printers
  services.avahi.nssmdns = true;
  programs.system-config-printer.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  services.udev = {
    enable = true;
    # Allow users in "video" group to control backlight.
    extraRules = ''
      SUBSYSTEM=="backlight", ACTION=="add", \
        RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", \
        RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thiago = {
    isNormalUser = true;
    description = "Thiago";
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "libvirt" "kvm" "video" ];
  };

  home-manager.users.thiago = { pkgs, ... }: {
    home.stateVersion = "22.05";
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
      keepassxc
      tdesktop # Telegram
      signal-desktop
      discord
      spotify
      tenacity # Audacity
      freerdp # Winapps
      bc # Winapps (basic calculator)

      # Programming
      vscodium
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
      name = "Emacs Client";
      comment = "Edit text";
      exec = "sh -c \"if [ -n \\\"\\$*\\\" ]; then exec emacsclient --alternate-editor= --display=\\\"\\$DISPLAY\\\" \\\"\\$@\\\"; else exec emacsclient --alternate-editor= --create-frame; fi\" placeholder %F";
      genericName = "Text Editor";
      icon = "emacs";
      settings = {
        Keywords = "Text;Editor;";
        StartupWMClass = "Emacs";
      };
      mimeType = [ "text/english" "text/plain" "text/x-makefile" "text/x-c++hdr" "text/x-c++src" "text/x-chdr" "text/x-csrc" "text/x-java" "text/x-moc" "text/x-pascal" "text/x-tcl" "text/x-tex" "application/x-shellscript" "text/x-c" "text/x-c++" ];
      terminal = false;
      type = "Application";
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
    
    # services.screen-locker = {
    #   enable = true;
    #   # security.wrappers.<name>.setuid
    #   lockCmd = "${pkgs.slock}/bin/slock";
    #   inactiveInterval = 1; # Minutes
    #   xautolock.enable = true;
    # };

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
      cursorTheme = {
        package = pkgs.callPackage /home/thiago/.config/myconfigs/nix/vimix-cursors/vimix-cursors.nix {};
        name = "Vimix-white-cursors";
      };
    };

    qt = {
      enable = true;
      platformTheme = "gtk";
      style.name = "gtk2";
    };

    # 漢語
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-rime fcitx5-chinese-addons ];
    };
  };

  environment.localBinInPath = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # CL
    nix-doc
    micro
    htop
    pfetch
    git
    unzip
    gnumake
    gcc
    acpilight

    # GUI
    simple-scan
    librewolf
    libreoffice-still
    thunderbird
    gimp
    kdenlive
    obs-studio
    celluloid
    virt-manager
  ];
  programs.java.enable = true;
  services.flatpak.enable = true;
  xdg.portal.enable = true; # Required for flatpak
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  programs.kdeconnect.enable = true;
  programs.slock.enable = true;

  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  fonts.fonts = with pkgs; [
    fira-code
    font-awesome
  ];

  # Games
  programs.steam.enable = true;
  # Wine games
  hardware.opengl.driSupport32Bit = true;
  services.samba.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # 443/631/9100-9102 = CUPS/Printers
  # 42000/42001 = Warpinator
  # 5353 = discovery protocol / mDNS
  networking.firewall = {
    allowedTCPPorts = [ 443 631 42000 42001 ];
    allowedUDPPorts = [ 5353 42000 42001 ];
    allowedTCPPortRanges = [
      { from = 9100; to = 9102; }
    ];
  };
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
