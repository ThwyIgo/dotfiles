{
  fileSystems."/nix".neededForBoot = true;

  disko.devices.nodev = {
    "/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=25%"
        "mode=755"
      ];
    };
  };

  disko.devices.disk.main = {
    # Use `readlink -f /dev/disk/by-id/{{...}}` to check if the id is correct
    device = "/dev/vda";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        esp = {
          name = "ESP";
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };

        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "crypted";
            # use `printf 'password' > luks-passwd.txt` to create the password file
            passwordFile = "/home/nixos/host/luks-passwd.txt";
            settings = {
              allowDiscards = true;
            };
            content = {
              type = "btrfs";
              extraArgs = ["-f"];

              subvolumes = {
                "/persistent" = {
                  mountOptions = ["subvol=persistent" "noatime" "compress=zstd"];
                  mountpoint = "/persistent";
                };

                "/nix" = {
                  mountOptions = ["subvol=nix" "noatime" "compress=zstd"];
                  mountpoint = "/nix";
                };

                "/home" = {
                  mountOptions = ["subvol=home" "compress=zstd"];
                  mountpoint = "/home";
                };

                "/var/lib/libvirt" = {
                  mountOptions = ["subvol=libvirt" "noatime" "compress=zstd"];
                  mountpoint = "/var/lib/libvirt";
                };

                "/swap" = {
                  mountpoint = "/.swapvol";
                  mountOptions = ["subvol=swap" "noatime" "nodatacow"];
                  # `free -g`
                  swap.swapfile.size = "4G";
                };
              };
            };
          };
        };
      };
    };
  };
}
