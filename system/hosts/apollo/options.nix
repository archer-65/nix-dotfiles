{pkgs, ...}: {
  system.modules = {
    credentials = {
      ssh.enable = true;
    };

    core = {
      cachix.enable = true;
    };

    shell = {
      bash.enable = true;
      zsh.enable = true;
    };
  };

  primaryUser.shell = pkgs.zsh;
}
