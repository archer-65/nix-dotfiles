{ pkgs, ... }:

{
  gtk = {
    enable = true;
    theme = {
      name = "Materia";
      package = pkgs.materia-theme;
    };
    iconTheme = {
      name = "Kora";
      package = pkgs.kora-icon-theme;
    };
  };

  home.packages = with pkgs; [
    lxappearance
  ];
}