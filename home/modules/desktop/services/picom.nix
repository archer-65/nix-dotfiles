_:
{ options, config, lib, ... }:

with lib;
let cfg = config.user-modules.desktop.services.picom;
in {
  options.user-modules.desktop.services.picom = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;

      backend = "glx";
      vSync = true;
      experimentalBackends = true;

      fade = true;
      fadeDelta = 4;
      fadeSteps = [ 0.028 0.03 ];

      activeOpacity = 1.0;
      inactiveOpacity = 1.0;

      # inactiveDim = 0;

      settings = {
        # Unredirect all windows if a full-screen opaque window is detected, to
        # maximize performance for full-screen windows. Known to cause
        # flickering when redirecting/unredirecting windows.
        unredir-if-possible = true;

        # GLX backend: Avoid using stencil buffer, useful if you don't have a
        # stencil buffer. Might cause incorrect opacity when rendering
        # transparent content (but never practically happened) and may not work
        # with blur-background. My tests show a 15% performance boost.
        # Recommended.
        glx-no-stencil = true;

        # Other
        mark-wmwin-focused = true;
        mark-ovredir-focused = false;
        use-ewmh-active-win = true;

        detect-rounded-corners = true;
        detect-client-opacity = true;

        focus-exclude = [
            "class_g = 'Cairo-clock'"
            "window_type = 'dock'"
            "! name~=''"
            "class_g = 'Dunst'"
            "class_g = 'Bar'"       # lemonbar
            "class_g = 'slop'"      # maim
        ];
        detect-transient = true;
        detect-client-leader = true;
      };
    };
  };
}
