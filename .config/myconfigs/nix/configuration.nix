{ config, pkgs, ... }:
{
  # Bootloader.
  boot.loader = {
    systemd-boot.enable = true;
    timeout = 0; # Press Space to show boot menu
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.hostName = "PeaceNixArch"; # Define your hostname.

  # Enable networking
  networking.networkmanager = {
    enable = true;
    #wifi.backend = "iwd";
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  # Garbage collection
  # nix.gc = {
  #   automatic = true;
  #   dates = "monthly";
  #   options = "--delete-older-than 7d";
  # };

  # Optimize nix store
  nix.settings.auto-optimise-store = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "pt_BR.UTF-8";
  i18n.extraLocales = [ "en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  services.xserver.displayManager.lightdm = {
    enable = true;
    greeters.slick = {
      enable = true;
      cursorTheme = {
        name = "Nordzy-cursors-white";
        package = pkgs.nordzy-cursor-theme;
      };
    };
  };

  programs.i3lock = {
    enable = true;
    package = pkgs.betterlockscreen;
  };

  services.xserver.xautolock = rec {
    enable = true;
    nowlocker = "${pkgs.betterlockscreen}/bin/betterlockscreen -l";
    time = 10;
    killtime = 20;
    locker = "${pkgs.xset}/bin/xset dpms force off;" + nowlocker;
    killer = "/run/current-system/systemd/bin/systemctl suspend";
  };

  # Enable Xmonad window manager
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  # services.desktopManager.cosmic.enable = false;
  # services.displayManager.cosmic-greeter.enable = false;

  # Auto-login
  services.displayManager = {
    defaultSession = "none+xmonad";
    autoLogin.enable = true;
    autoLogin.user = "thiago";
  };

  # systemd.targets = {
  #   sleep.enable = false;
  #   suspend.enable = false;
  #   hibernate.enable = false;
  #   hybrid-sleep.enable = false;
  # };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "br";
    variant = "";
  };

  # Configure console keymap
  console.keyMap = "br-abnt2";

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };
  services.avahi = {
    enable = true; # Auto-detect printers
    nssmdns4 = true;
    openFirewall = true;
  };
  programs.system-config-printer.enable = true;
  # Enable scanners
  hardware.sane.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
  };
  security.pam.loginLimits = [
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
  ];

  services.udev = {
    enable = true;
    # Allow users in "video" group to control backlight.
    extraRules = ''
      SUBSYSTEM=="backlight", ACTION=="add", \
        RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", \
        RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
  };

  # Allows applications to query and manipulate storage devices, e.g. automount
  services.udisks2.enable = true;
  services.devmon.enable = true;
  # Virtual filesystem. Make trash work in file managers
  services.gvfs.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thiago = {
    isNormalUser = true;
    description = "Thiago";
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "docker" "video" "audio" "scanner" "lp" ];
  };

  home-manager.users.thiago = import ./home-manager/thiago.nix;

  environment.localBinInPath = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # CL
    xinit
    alacritty
    micro
    htop
    git
    unzip
    acpilight
    xkill
    killall
    ffmpegthumbnailer

    # Sysadmin
    ntfs3g
    arandr
  ];
  programs.kdeconnect.enable = true;

  # services.flatpak.enable = true;
  # xdg.portal.enable = true; # Required for flatpak
  # xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  virtualisation.libvirtd = {
    enable = true;
    qemu.vhostUserPackages = with pkgs; [ virtiofsd ];
  };
  programs.dconf.enable = true;
  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  # Games
  programs.steam.enable = true;
  programs.gamemode.enable = true;
  # Wine games
  # hardware.opengl.driSupport32Bit = true;
  # services.samba.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    #enableSSHSupport = true;
  };
  services.pcscd.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # 443/631/9100-9102 = CUPS/Printers
  # 5353 = discovery protocol / mDNS
  networking.firewall = {
    allowedTCPPorts = [];
    allowedTCPPortRanges = [];
  };
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  security.polkit.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
