{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.emacs;
  configDir = "${config.home.homeDirectory}/.dotfiles/home/mario/modules/editors/emacs/config";
in {
  options.mario.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
    telega.enable = mkEnableOption "telegram client";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        hack-font

        hunspell
        hunspellDicts.it_IT
        hunspellDicts.en_US
      ];

      programs.emacs = {
        enable = true;
        package =
          if pkgs.stdenv.isDarwin
          then pkgs.emacs-macport
          else pkgs.emacs29-pgtk;

        extraPackages = epkgs:
          [
            epkgs.treesit-grammars.with-all-grammars
            epkgs.vterm
            epkgs.pdf-tools
          ]
          ++ (optionals cfg.telega.enable [
            epkgs.melpaPackages.telega
          ]);
      };

      xdg.configFile."emacs/Emacs.org".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/Emacs.org";
      xdg.configFile."emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/init.el";
      xdg.configFile."emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/early-init.el";
      xdg.configFile."emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/lisp";
      xdg.configFile."emacs/img".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/img";
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        startWithUserSession = "graphical";
      };

      home.sessionVariables = {
        EDITOR = "emacsclient -t";
        VISUAL = "emacsclient -c -a emacs";
      };
    })

    (mkIf config.programs.notmuch.enable {
      home.packages = [pkgs.notmuch.emacs];
    })
  ]);
}
