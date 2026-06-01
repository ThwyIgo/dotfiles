# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.channel.enable = false;
  nix.settings.auto-optimise-store = true;
  environment.localBinInPath = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 0; # Press Space to show boot menu
  boot.initrd.luks.devices = {
    cryptroot = {
      # Obtenha o UUID com `blkid /dev/vda2 -s PARTUUID`
      device = "/dev/disk/by-partuuid/975b7376-5b01-4991-89ae-27c12eae4442";
      preLVM = true;
    };
  };
  zramSwap.enable = true;
  console.keyMap = "br-abnt2";

  networking.hostName = "Thiago-ambulante"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };
  
  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "pt_BR.UTF-8";

  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.open = true;
  # Esta opção recompila vários pacotes com suporte a CUDA.
  # Provavelmente os nixos-rebuild vão demorar várias horas.
  #nixpkgs.config.cudaSupport = true;

  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.enable = true;
  
  services.displayManager.autoLogin = {
    enable = true;
    user = "thiago";
  };

  # Enable CUPS to print documents.
  #services.printing = {
  #  enable = true;
  #  drivers = [ pkgs.hplipWithPlugin ];
  #};
  #services.avahi = {
  #  enable = true; # Auto-detect printers
  #  nssmdns4 = true;
  #  openFirewall = true;
  #};
  # Enable scanners
  #hardware.sane.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;
  
  # Não mostrar mensagem de "boas vindas" do sudo
  security.sudo.extraConfig = "Defaults  lecture=\"never\"";

  # Não faz sentido os usuário serem mutáveis se tudo sempre será apagado
  users.mutableUsers = false;
  users.users.thiago = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "kvm" "libvirtd" "scanner" "lp" ];
    initialHashedPassword = "$6$vnrhXRoJm51Ett5l$KIFeOjqf2d/OomhNkBIsRjde0s9vkUqZmWLRHtgthAy6wkGp8mb3LhnfVYBBUAYmn.8EBx7FMQmCCetSCE9.J1";
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = super: {
    obs-studio = super.obs-studio.override { cudaSupport = true; };
  };

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    # CL
    micro-with-wl-clipboard
    git
    unzip
    killall
  
    # Sysadmin
    ntfs3g
  ];
  programs = {
    kdeconnect.enable = true;
    htop.enable = true;
  };

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

  programs.dconf.enable = true;
  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
    #storageDriver = "btrfs";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  services.mongodb = {
    enable = true;
  	package = pkgs.mongodb-ce;
  	bind_ip = "0.0.0.0";
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_18;
    extensions = ps: with ps; [ timescaledb ];
    settings.shared_preload_libraries = "timescaledb";
    authentication = ''
        # Tipo  Banco   Usuário  Endereço        Método
        local   all     root    trust
        host    all     root    127.0.0.1/32   md5
        host    all     all     0.0.0.0/0      md5
        host    all     all     ::1/128        md5
      '';
    initialScript = pkgs.writeText "init-sql-script" ''
      ALTER USER postgres WITH PASSWORD '1234';
      CREATE DATABASE portal;
      CREATE DATABASE keycloak;
      CREATE USER keycloak WITH PASSWORD 'keycloak';
      GRANT ALL PRIVILEGES ON DATABASE keycloak TO keycloak;
      CREATE EXTENSION IF NOT EXISTS dblink;
      SELECT dblink_exec('dbname=keycloak', 'GRANT ALL PRIVILEGES ON SCHEMA public TO keycloak');
    '';
  };

  services.keycloak = {
  	enable = true;
  	initialAdminPassword = "admin";
  	settings = {
  	  http-port = 8081;
  	  hostname = "localhost";
  	  http-enabled = true;
  	  hostname-strict-https = false;
  	};
  	database = {
      createLocally = false;
      passwordFile = let drv = pkgs.writeText "PostgreSQL-password" "keycloak";
                     in builtins.seq (builtins.readFile drv) drv.outPath;
  	};
    realmFiles = [
      ./config/portal-realm.json
  	];
  };

  services.apache-kafka = {
  	enable = true;
  	clusterId = "ButnusLcSVKOO3Bw0950Uw";
  	formatLogDirs = true;
  	  settings = {
  	    listeners = [
  	      "PLAINTEXT://0.0.0.0:9092"
  	      "CONTROLLER://0.0.0.0:9093"
  	    ];
  	    "advertised.listeners" = [
  	      "PLAINTEXT://10.3.192.74:9092" 
  	    ];
  	    "listener.security.protocol.map" = [
  	      "PLAINTEXT:PLAINTEXT"
  	      "CONTROLLER:PLAINTEXT"
  	    ];
  	    "controller.quorum.voters" = [
  	      "1@10.3.192.74:9093"
  	    ];
  	    "controller.listener.names" = [ "CONTROLLER" ];
  	    "node.id" = 1;
  	    "process.roles" = [ "broker" "controller" ];
        "log.dirs" = [ "/var/lib/apache-kafka" ];
        "offsets.topic.replication.factor" = 1;
        "transaction.state.log.replication.factor" = 1;
        "transaction.state.log.min.isr" = 1;
     };
  };
  # Set this so that systemd automatically create /var/lib/apache-kafka with the right permissions
  systemd.services.apache-kafka.unitConfig.StateDirectory = "apache-kafka";

  environment.sessionVariables = {
    LIBVIRT_DEFAULT_URI = "qemu:///system";
    PHYSIS_MONGO_DB = "mongodb://localhost";
    KAFKA_BROKER_1 = "localhost:9092";
    PHYSIS_POSTGRESQL = "Server=localhost;Port=5432;Database=postgres;User Id=postgres;Password=1234;";
    Keycloak__ClientSecret = "xGNa1jvTDtRlnxguAdqebGH3EQCfXMD7";
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      AllowUsers = [ "thiago" ];
    };
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 8080 ];
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 60000;
      to = 60999;
    }
  ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

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
  system.stateVersion = "25.05"; # Did you read the comment?
}
