# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  #### NIX CONFIG ####

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.channel.enable = false;
  nix.settings.auto-optimise-store = true;

  #### BOOTLOADER CONFIG ####

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 0; # Press Space to show boot menu
  # Hibernation support
  # To find the resume_offset, run:
  # sudo btrfs inspect-internal map-swapfile -r /.swapvol/swapfile
  # boot.resumeDevice = "/dev/mapper/crypted";
  # boot.kernelParams = [ "resume_offset=XXXXX" ];
  boot.initrd.systemd.enable = true;

  #### NETWORK CONFIG ####

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };
  networking.firewall = {
    allowedTCPPorts = [ ];
    allowedUDPPorts = [ ];
  };

  #### LOCALIZATION CONFIG ####

  console.keyMap = "br-abnt2";
  time.timeZone = "America/Sao_Paulo";
  i18n.defaultLocale = "pt_BR.UTF-8";

  #### DRIVER CONFIG ####

  zramSwap.enable = true;

  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  #### SOFTWARE CONFIG ####

  environment.localBinInPath = true;

  # Don't show sudo welcome message
  security.sudo.extraConfig = "Defaults  lecture=\"never\"";

  #### USERS CONFIG ####

  users.mutableUsers = false;
  # "user" is the name of the user
  users.users.user = {
    description = "";
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "libvirtd" ];
    # mkpasswd -m sha-512 | sudo tee /mnt/persistent/etc/nixos/secrets/user-passwd.txt
    hashedPasswordFile = "/etc/nixos/secrets/user-passwd.txt";
  };
  home-manager.users.user = ./home-manager/user.nix;

  #### SYSTEM PKGS CONFIG ####

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    killall
  ];
  programs = {
    htop.enable = true;
  };

  #### SERVICES CONFIG  ####

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      AllowUsers = [ "user" ];
    };
  };

  #### VIRTUALIZATION CONFIG ####

  virtualisation.libvirtd = {
    enable = true;
    qemu.vhostUserPackages = [ pkgs.virtiofsd ];
    qemu.swtpm.enable = true;
  };
  systemd.services.libvirt-default-network = {
    description = "Start libvirt default network";
    after = ["libvirtd.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.libvirt}/bin/virsh net-start default";
      ExecStop = "${pkgs.libvirt}/bin/virsh net-destroy default";
      User = "root";
    };
  };
  environment.sessionVariables.LIBVIRT_DEFAULT_URI = "qemu:///system";

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?
}
