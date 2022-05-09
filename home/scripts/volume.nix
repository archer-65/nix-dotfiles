{ pkgs, ... }:

pkgs.writeShellScriptBin "volume" ''
  # Script to adjust brightness and show an indicator using dunst

  # Call it like this:
  # $./volume.sh up
  # $./volume.sh down
  # $./volume.sh mute

  #  Requirements:
  #  dunst (obviously)
  #  dunstify
  #  icon theme (you can change dir)

  #dir="$HOME/.icons/rose-pine-moon-icons/48x48/status/notification-audio-volume"

  # Function to repeat a character (because seq is ugly)
  # arg $1: number of repetitions
  # arg $2: char to be printed
  function repChar {
    for (( i = 0; i < $1; i++ )); do
      printf "$2"
    done
  }

  function get_volume {
    pamixer --get-volume
  }

  function is_mute {
    pamixer --get-mute > /dev/null
  }

  # Bar is printed with a fixed width and a padding character ("░")
  # so it can be used in a dynamically sized dunst frame and
  # is therefore at least somewhat portable between hidpi
  # and normal screens
  function send_notification {
    volume=`get_volume`

    length=20 # Number characters for the bar
    div=$((100 / $length))
    total=$((100 / $div))
    left=$(($volume / $div))
    right=$(($total - $left))
    bar=$(repChar $left "█")$(repChar $right "░")

    # Send the notification
    #dunstify -I "$icon" -r 2593 -u normal "$bar"
    dunstify -i "$icon" -r 2593 -u normal "$bar"
    #dunstify -i NUL -r 2593 -u normal "  $bar"
  }

  case $1 in
    up)
      # Icon
      #icon="''${dir}-high.svg"  
      icon=audio-volume-high
      
      # Set the volume on (if it was muted)
      pamixer -u > /dev/null
      # Up the volume (+ 5%)
      pamixer -i 5 > /dev/null
      
      send_notification
      ;;
    down)
      # Icon
      #icon="''${dir}-low.svg"  
      icon=audio-volume-low  

      # Set the volume on (if it was muted)
      pamixer -u > /dev/null
      # Up the volume (+ 5%)
      pamixer -d 5 > /dev/null
      
      send_notification
      ;;
    mute)
      # Toggle mute
      pamixer -t > /dev/null
      if is_mute ; then
        #icon="''${dir}-muted.svg"
        #dunstify -I "$icon" -r 2593 -u normal "Muted"
        icon=audio-volume-muted
        dunstify -i "$icon" -r 2593 -u normal "Muted"
      else
        #icon="''${dir}-high.svg"
        icon=audio-volume-high
        send_notification
      fi
      ;;
  esac
''