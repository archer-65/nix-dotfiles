{ pkgs, ...}:

{
  boot = {
    consoleLogLevel = 0;
    initrd.verbose = false;
    plymouth.enable = true;
    kernelParams = [ 
      "quiet" 
      "splash"
      "boot.shell_on_fail" 
      "udev.log_priority=3" 
      "loglevel=3" 
      "rd.systemd.show_status=false" 
      "rd.udev.log_level=3" 
    ];
  };		
}
