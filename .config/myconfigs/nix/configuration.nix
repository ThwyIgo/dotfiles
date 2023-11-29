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
  boot.loader = {
    systemd-boot.enable = true;
    timeout = 0; # Press Space to show boot menu
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
  };

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
    xplayer
    cinnamon.xreader
    cinnamon.xviewer
    cinnamon.warpinator
    gnome.gnome-calendar
  ];
  programs = {
    geary.enable = false;
    gnome-terminal.enable = false;
  };

  # Enable Xmonad window manager
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  # Auto-login
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "thiago";

  # Configure keymap in X11
  services.xserver = {
    layout = "br";
    xkbVariant = "";
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
    nssmdns = true;
    openFirewall = true;
  };
  programs.system-config-printer.enable = true;
  # Enable scanners
  hardware.sane.enable = true;

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
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "docker" "video" "scanner" "lp" ];
  };

  fonts.fontconfig.enable = true;

  home-manager.users.thiago = import (/home/thiago +
                                      /.config/myconfigs/nix/home-manager-config.nix);

  environment.localBinInPath = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # CL
    alacritty
    micro
    htop
    pfetch
    git
    unzip
    gnumake
    gcc
    acpilight
    xorg.xkill

    # Sysadmin
    virtiofsd
    arandr
    pavucontrol
    xfce.xfce4-taskmanager

    # GUI
    simple-scan
    librewolf
    libreoffice-still
    thunderbird
    gimp
    kdenlive
    obs-studio
    celluloid
  ];
  programs.kdeconnect.enable = true;
  programs.slock.enable = true;

  # services.flatpak.enable = true;
  # xdg.portal.enable = true; # Required for flatpak
  # xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  fonts.fonts = with pkgs; [
    font-awesome
    fira-code
  ];

  # Games
  programs.steam.enable = true;
  # Wine games
  # hardware.opengl.driSupport32Bit = true;
  # services.samba.enable = true;

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
  # 5353 = discovery protocol / mDNS
  networking.firewall = {
    allowedTCPPorts = [];
    allowedTCPPortRanges = [];
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
