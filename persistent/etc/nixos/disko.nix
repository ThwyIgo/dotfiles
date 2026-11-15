{
  fileSystems."/nix".neededForBoot = true;
  fileSystems."/persistent".neededForBoot = true;

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
            passwordFile = "/tmp/luks-passwd.txt";
            settings = {
              allowDiscards = true;
              crypttabExtraOpts = [
                "tpm2-device=auto"
                "tpm2-measure-pcr=yes"
              ];
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

  disko.devices.disk.home = {
    device = "/dev/vdb";
    type = "disk";
    content = {
      type = "gpt";
      partitions = {
        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "crypted-home";
            passwordFile = "/tmp/luks-passwd.txt";
            settings = {
              allowDiscards = true;
              # Skip initrd unlock so stage 2 /etc/crypttab unlocks it after /persistent is mounted
              crypttabExtraOpts = [ "noauto" ];
            };
            content = {
              type = "btrfs";
              extraArgs = ["-f"];

              subvolumes = {
                "/home" = {
                  mountOptions = ["subvol=home" "compress=zstd"];
                  mountpoint = "/home";
                };
              };
            };
          };
        };
      };
    };
  };
}
