_:
{ lib, config, pkgs, ... }:

{
  # Timezone
  time.timeZone = "Europe/Rome";

  # Locale
  i18n.defaultLocale = "en_US.UTF-8";

  # Console settings
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
}
