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
      package = pkgs.bibata-cursors;
    };
  };

  home.packages = with pkgs; [
    lxappearance
  ];
}
