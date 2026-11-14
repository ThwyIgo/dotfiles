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
  nix.settings.allowed-users = [ "@wheel" ];

  #### BOOTLOADER CONFIG ####

  # Use the Limine EFI boot loader.
  boot.loader.limine = {
    enable = true;
    enableEditor = false;
    secureBoot = {
      enable = true;
      autoGenerateKeys = true;
      autoEnrollKeys = {
        enable = true;
        # Omit --firmware-builtin if the motherboard lack dbDefault (as OVMF and some boards)
        extraArgs = [ "--microsoft" "--firmware-builtin" ];
      };
    };
    # Hide menu unless a key is pressed during the timeout window
    extraConfig = ''
      quiet: yes
    '';
    # If testing in a UEFI VM without persistent NVRAM, set:
    # efiInstallAsRemovable = true;
  };
  # Set to false if using efiInstallAsRemovable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 1;

  # Enable IOMMU, strict DMA protection, and TPM2 PCR 15 volume-key measurement
  boot.kernelParams = [
    "intel_iommu=on"
    "amd_iommu=force_isolation"
    "iommu.passthrough=0"
    "iommu.strict=1" # Remove if I/O performance is slow. You'll be vulnerable to some very sophisticated attacks.
    "efi=disable_early_pci_dma" # Remove if any PCI device fails to initialize at boot
    "systemd.tpm2_measured_os=1"
  ];

  #### SECURITY / HARDENING CONFIG ####

  security.apparmor.enable = true;

  boot.kernel.sysctl = {
    # Prevent replacing the running kernel without breaking hibernation
    "kernel.kexec_load_disabled" = 1;

    # Hide kernel pointers and restrict kernel logs
    "kernel.kptr_restrict" = 2;
    "kernel.dmesg_restrict" = 1;

    # Restrict ptrace to parent/child processes
    "kernel.yama.ptrace_scope" = 1;

    # Disable unprivileged eBPF and harden BPF JIT
    "kernel.unprivileged_bpf_disabled" = 1;
    "net.core.bpf_jit_harden" = 2;

    # Filesystem and core dump hardening
    "fs.protected_symlinks" = 1;
    "fs.protected_hardlinks" = 1;
    "fs.protected_fifos" = 2;
    "fs.protected_regular" = 2;
    "fs.suid_dumpable" = 0;

    # Network stack hardening
    "net.ipv4.conf.all.accept_redirects" = 0;
    "net.ipv4.conf.default.accept_redirects" = 0;
    "net.ipv6.conf.all.accept_redirects" = 0;
    "net.ipv6.conf.default.accept_redirects" = 0;
    "net.ipv4.conf.all.send_redirects" = 0;
    "net.ipv4.conf.default.send_redirects" = 0;
    "net.ipv4.icmp_echo_ignore_broadcasts" = 1;
    "net.ipv4.tcp_syncookies" = 1;
    # Loose reverse path filtering (2) preserves compatibility with VPNs and libvirt bridges
    "net.ipv4.conf.all.rp_filter" = 2;
    "net.ipv4.conf.default.rp_filter" = 2;
  };

  #### NETWORK CONFIG ####

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd"; # If wifi is not working, change this to "wpa_supplicant"
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

  # Don't show sudo welcome message and restrict sudo execution to wheel
  security.sudo.extraConfig = "Defaults  lecture=\"never\"";
  security.sudo.execWheelOnly = true;

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
    sbctl
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
  system.stateVersion = "26.11"; # Did you read the comment?
}
