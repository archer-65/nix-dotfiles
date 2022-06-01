{ pkgs, ... }:

{
  gtk = {
    enable = true;

    theme = {
      name = "Materia-dark-compact";
      package = pkgs.materia-theme;
    };

    iconTheme = {
      name = "kora";
      package = pkgs.kora-icon-theme;
    };

    cursorTheme = {
      name = "Bibata-Modern-Ice";
      size = 16;
      package = pkgs.bibata-cursors;
    };

    font = { name = "VictorMono Nerd Font 12"; };
  };

  home.packages = with pkgs; [ lxappearance ];
}
