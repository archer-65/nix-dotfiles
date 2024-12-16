{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  nix = {
    package = pkgs.nixVersions.latest;

    settings = {
      trusted-users = ["root" "@wheel" "nix-builder"];
      allowed-users = ["root" "@wheel" "nix-builder"];

      auto-optimise-store = true;

      # let the system decide the number of max jobs
      max-jobs = "auto";

      # build inside sandboxed environments
      sandbox = true;
      sandbox-fallback = false;

      # supported system features
      system-features = ["nixos-test" "kvm" "recursive-nix" "big-parallel"];

      # extra architectures supported by my builders
      extra-platforms = config.boot.binfmt.emulatedSystems;

      # continue building derivations if one fails
      keep-going = true;

      # bail early on missing cache hits
      connect-timeout = 5;

      # show more log lines for failed builds
      log-lines = 30;

      # enable new nix command and flakes
      # and also "unintended" recursion as well as content addressed nix
      extra-experimental-features = [
        "flakes" # flakes
        "nix-command" # experimental nix commands
        "recursive-nix" # let nix invoke itself
        "ca-derivations" # content addressed nix
        "auto-allocate-uids" # allow nix to automatically pick UIDs, rather than creating nixbld* user accounts
        "configurable-impure-env" # allow impure environments
        "cgroups" # allow nix to execute builds inside cgroups
        "git-hashing" # allow store objects which are hashed via Git's hashing algorithm
        "verified-fetches" # enable verification of git commit signatures for fetchGit
      ];

      warn-dirty = false;

      # maximum number of parallel TCP connections used to fetch imports and binary caches, 0 means no limit
      http-connections = 50;

      # whether to accept nix configuration from a flake without prompting
      accept-flake-config = false;

      # execute builds inside cgroups
      use-cgroups = true;

      # for direnv GC roots
      keep-outputs = true;
      keep-derivations = true;
    };

    gc = {
      automatic = true;
      dates = "Tue *-*-* 18:00:00";
      options = "--delete-older-than 14d";
    };

    optimise = {
      automatic = true;
      dates = ["19:00"];
    };

    # make builds run with low priority so my system stays responsive
    # this is especially helpful if you have auto-upgrade on
    daemonCPUSchedPolicy = "batch";
    daemonIOSchedClass = "idle";
    daemonIOSchedPriority = 7;

    # Add each flake input as a registry
    # To make nix3 commands consistent with the flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # Map registries to channels
    # Very useful when using legacy commands
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
  };

  # https://www.reddit.com/r/NixOS/comments/1enj0ab/note_to_self_do_not_collect_garbage_and_optimise
  systemd.services.nix-optimise = {
    after = ["nix-gc.service"];
  };

  # https://github.com/NotAShelf/nyx/blob/d407b4d6e5ab7f60350af61a3d73a62a5e9ac660/modules/core/common/system/nix/module.nix#L236-L244
  systemd.services.nix-gc = {
    unitConfig.ConditionACPower = true;
  };
}
