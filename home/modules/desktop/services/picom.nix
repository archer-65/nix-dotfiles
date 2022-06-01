{ pkgs, ... }:

{
  services.picom = {
    enable = true;

    backend = "glx";
    vSync = true;
    experimentalBackends = true;

    fade = true;
    fadeDelta = 4;
    fadeSteps = [ "0.028" "0.03" ];

    activeOpacity = "1.0";
    inactiveOpacity = "1.0";

    inactiveDim = "0";

    extraOptions = ''
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
          "class_g = 'Cairo-clock'",
          "window_type = 'dock'",		
          "! name~='''",
          "class_g = 'Dunst'",
          "class_g = 'Bar'",                    # lemonbar
          "class_g = 'slop'"                    # maim
      ];
      detect-transient = true;
      detect-client-leader = true;
    '';

    ### Uncomment this lines to enable ibhagwan fork of picom
    # package = pkgs.picom.overrideAttrs (old: {
    #   src = pkgs.fetchFromGitHub {
    #   repo = "picom";
    #   owner = "ibhagwan";
    #   rev = "next-rebase";
    #   sha256 = "1hVFBGo4Ieke2T9PqMur1w4D0bz/L3FAvfujY9Zergw=";
    #   };
    # }); 
  };

  ## Uncomment this to disable picom service
  # systemd.user.services.picom = pkgs.lib.mkForce {}; 
}
