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
  };

  home.packages = with pkgs; [
    lxappearance
  ];
}
