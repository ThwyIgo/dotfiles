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
        }
      ];
    };
  };
}
