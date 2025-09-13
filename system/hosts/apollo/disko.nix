{ disks, ... }:
{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = builtins.elemAt disks 0;
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = [ "-f" ];
                subvolumes = {
                  "@" = {
                    mountpoint = "/";
                    mountOptions = [
                      "compress=zstd"
                      "autodefrag"
                      "space_cache=v2"
                      "noatime"
                      "compress=zstd"
                      "discard=async"
                    ];
                  };
                  "@home" = {
                    mountpoint = "/home";
                    mountOptions = [
                      "compress=zstd"
                      "autodefrag"
                      "space_cache=v2"
                      "noatime"
                      "compress=zstd"
                      "discard=async"
                    ];
                  };
                  "@nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd"
                      "autodefrag"
                      "space_cache=v2"
                      "noatime"
                      "compress=zstd"
                      "discard=async"
                    ];
                  };
                  "@var_log" = {
                    mountpoint = "/var/log";
                    mountOptions = [
                      "compress=zstd"
                      "autodefrag"
                      "space_cache=v2"
                      "noatime"
                      "compress=zstd"
                      "discard=async"
                    ];
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
