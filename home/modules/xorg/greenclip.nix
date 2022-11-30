{
  pkgs,
  config,
  lib,
  options,
  ...
}:
with lib; let
  cfg = config.mario.modules.xorg.greenclip;
in {
  options.mario.modules.xorg.greenclip = {
    enable = mkEnableOption "greenclip support";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      haskellPackages.greenclip
      rofi-plugins.greenclip
    ];

    xdg.configFile."greenclip.toml".text = ''
      [greenclip]
      blacklisted_applications = []
      enable_image_support = false
      history_file = "/home/mario/.cache/greenclip.history"
      image_cache_directory = "/tmp/greenclip"
      max_history_length = 50
      max_selection_size_bytes = 0
      trim_space_from_selection = true
      use_primary_selection_as_input = true
      static_history = [
       '''¯\_(ツ)_/¯''',
      ]
    '';

    systemd.user.services.greenclip = {
      Unit = {
        Description = "greenclip daemon";
        After = ["graphical-session.target"];
      };
      Install = {WantedBy = ["graphical-session.target"];};
      Service = {
        ExecStart = "${pkgs.haskellPackages.greenclip}/bin/greenclip daemon";
      };
    };
  };
}
