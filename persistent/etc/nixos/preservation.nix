{
  preservation = {
    enable = true;

    preserveAt."/persistent" = {
      directories = [
        {
          directory = "/etc/nixos";
          inInitrd = true;
        }
        {
          # NixOS dynamically generates the sshd_config file and places
          # a symlink in /etc/ssh during the activation phase. Later in the boot process, the persistent /etc/ssh directory is mounted over the
          # actual /etc/ssh directory. This hides the dynamically generated sshd_config symlink, which causes the SSH daemon to fail with the
          # error: /etc/ssh/sshd_config: No such file or directory.
          # inInitrd = true; fixes the problem above
          directory = "/etc/ssh";
          inInitrd = true;
        }
        "/etc/NetworkManager/system-connections"
        "/var/lib/iwd"
        "/var/lib/bluetooth"
        "/var/lib/systemd/backlight"
        "/var/lib/systemd/timers"
        "/var/log"
        {
          directory = "/var/lib/nixos";
          inInitrd = true;
        }
      ];

      files = [
        {
          file = "/etc/machine-id";
          inInitrd = true;
          how = "symlink";
          configureParent = true;
        }
      ];
    };
  };

  # systemd-machine-id-commit.service would fail, but it is not relevant
  # in this specific setup for a persistent machine-id so we disable it
  systemd.suppressedSystemUnits = [ "systemd-machine-id-commit.service" ];
  # let the service commit the transient ID to the persistent volume
  systemd.services.systemd-machine-id-commit = {
    unitConfig.ConditionPathIsMountPoint = [
      ""
      "/persistent/etc/machine-id"
    ];
    serviceConfig.ExecStart = [
      ""
      "systemd-machine-id-setup --commit --root /persistent"
    ];
  };

  boot.initrd.systemd.services.init-machine-id = {
    description = "Initialize machine-id for preservation";
    wantedBy = [ "initrd-root-fs.target" ];
    after = [ "sysroot-persistent.mount" ];
    unitConfig.DefaultDependencies = false;
    serviceConfig.Type = "oneshot";
    script = ''
      mkdir -p /sysroot/persistent/etc
      if [ ! -f /sysroot/persistent/etc/machine-id ]; then
        echo "uninitialized" > /sysroot/persistent/etc/machine-id
      fi
    '';
  };
}
