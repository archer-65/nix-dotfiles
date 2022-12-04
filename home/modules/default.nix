{
  mario = {
    "credentials/gpg" = import ./credentials/gpg.nix;
    "credentials/mail" = import ./credentials/mail.nix;
    "credentials/bitwarden" = import ./credentials/bitwarden.nix;

    "xorg" = import ./xorg;
    "xorg/qtile" = import ./xorg/qtile;
    "xorg/greenclip" = import ./xorg/greenclip.nix;
    "xorg/autorandr" = import ./xorg/autorandr.nix;
    "xorg/picom" = import ./xorg/picom.nix;
    "xorg/locker" = import ./xorg/locker.nix;

    "wayland" = import ./wayland;
    "wayland/sway" = import ./wayland/sway.nix;
    "wayland/hyprland" = import ./wayland/hyprland.nix;
    "wayland/locker" = import ./wayland/locker.nix;
    "wayland/waybar" = import ./wayland/waybar.nix;

    "apps/dunst" = import ./apps/dunst.nix;
    "apps/rofi" = import ./apps/rofi;
    "apps/discord" = import ./apps/discord.nix;
    "apps/teams" = import ./apps/teams.nix;

    "browsers/firefox" = import ./browsers/firefox.nix;
    "browsers/chromium" = import ./browsers/chromium.nix;

    "gaming/emulators" = import ./gaming/emulators.nix;

    "graphical" = import ./graphical;

    "media/documents" = import ./media/documents.nix;
    "media/videos" = import ./media/videos.nix;

    "term" = import ./term;
    "term/alacritty" = import ./term/alacritty.nix;

    "dev/cc" = import ./dev/cc.nix;
    "dev/js" = import ./dev/js;
    "dev/java" = import ./dev/java.nix;
    "dev-lang.nix" = import ./dev/nix-lang.nix;
    "dev/python" = import ./dev/python.nix;
    "dev/rust" = import ./dev/rust.nix;
    "dev/terraform" = import ./dev/terraform.nix;
    "dev/tex" = import ./dev/tex.nix;

    "editors/android-studio" = import ./editors/android-studio.nix;
    "editors/emacs" = import ./editors/emacs;
    "editors/intellij" = import ./editors/intellij.nix;
    "editors/neovim" = import ./editors/neovim.nix;
    "editors/vscode" = import ./editors/vscode.nix;

    "shell/bash" = import ./shell/bash.nix;
    "shell/direnv" = import ./shell/direnv.nix;
    "shell/extensions" = import ./shell/extensions.nix;
    "shell/git" = import ./shell/git.nix;
    "shell/starship" = import ./shell/starship.nix;

    "themes" = import ./themes;
    "themes/bar.nix" = import ./themes/bar.nix;
    "themes/fonts" = import ./themes/fonts.nix;
    "themes/onedark" = import ./themes/onedark.nix;
    "themes/modus" = import ./themes/modus.nix;
  };
}
