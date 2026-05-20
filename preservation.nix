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
