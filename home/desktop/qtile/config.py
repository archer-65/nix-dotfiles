from os import path
import subprocess
import platform

from typing import List 

from libqtile import bar, layout, widget, hook, qtile
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy

from settings.theme import colors

#####################
##### VARIABLES #####
#####################

##### IMPORTANT #####

# Modifiers
mod         = "mod4"

# Get hostname for settings
hostname = platform.uname().node

# Expand every important dir
home       = path.expanduser('~')
scripts    = path.join(home, ".local", "bin")
qtile_path = path.join(home, ".config", "qtile")

# Rofi launchers
launcher    = path.join(scripts, "rofi_launcher")
powermenu   = path.join(scripts, "rofi_powermenu")
clipboard   = path.join(scripts, "rofi_clipboard")
emoji       = path.join(scripts, "rofi_emoji")

# Media scripts
volume      = path.join(scripts, "volume")

# Programs
terminal    = "alacritty"
filemanager = "thunar"
browser     = "firefox"
editor      = "emacsclient -c -a emacs"
mail        = editor + " --eval '(mu4e)'"
#git         = editor + " --eval '(magit)'"
gitgui      = "github-desktop"
pdfview     = "zathura"

# Widget commands for callback
update      = terminal + " -e sudo pacman -Syu"
mixer       = terminal + " -e pulsemixer"

# Call autostart script
@hook.subscribe.startup_once
def autostart():
    subprocess.call([path.join(qtile_path, "autostart.sh")])
    
####################
##### KEYBINDS #####
####################

keys = [
    #########################
    ##### QTILE CONTROL #####
    #########################
    
    # Switch between windows
    Key([mod], "h",
        lazy.layout.left(),
        desc="Move focus to left"),

    Key([mod], "l",
        lazy.layout.right(),
        desc="Move focus to right"),

    Key([mod], "j",
        lazy.layout.down(),
        desc="Move focus down"),

    Key([mod], "k",
        lazy.layout.up(),
        desc="Move focus up"),

    Key([mod], "space",
        lazy.layout.next(),
        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h",
        lazy.layout.swap_left(),
        lazy.layout.swap_column_left(),
        lazy.layout.shuffle_left().when(layout="bsp"),
        desc="Move window to the left or swap column"),
    
    Key([mod, "shift"], "l",
        lazy.layout.swap_right(),
        lazy.layout.swap_column_right(),
        lazy.layout.shuffle_right().when(layout="bsp"),
        desc="Move window to the right or swap column"),
    
    Key([mod, "shift"], "j",
        lazy.layout.shuffle_down() ,
        desc="Move window down"),
    
    Key([mod, "shift"], "k",
        lazy.layout.shuffle_up(),
        desc="Move window up"),
    
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        desc="Grow window to the left"),
    
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        desc="Grow window to the right"),
    
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        desc="Grow window down"),
    
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        desc="Grow window up"),
    
    Key([mod], "n",
        lazy.layout.normalize(),
        desc="Reset all window sizes (secondary clients)"),

    # Monad
    Key([mod], "equal",
        lazy.layout.grow(),
        desc="Grow Monad"),
    
    Key([mod], "minus",
        lazy.layout.shrink(),
        desc="Shrink Monad"),
    
    Key([mod, "shift"], "space",
        lazy.layout.flip(),
        desc="Flip layout"),

    Key([mod], "m",
        lazy.layout.maximize(),
        desc="Toggle maximize for focused"),

    # BSP
    Key([mod, "mod1"], "j",
        lazy.layout.flip_down(),
        desc="Flip BSP down"),
    
    Key([mod, "mod1"], "k",
        lazy.layout.flip_up(),
        desc="Flip BSP up"),

    Key([mod, "mod1"], "h",
        lazy.layout.flip_left(),
        desc="Flip BSP left"),

    Key([mod, "mod1"], "l",
        lazy.layout.flip_right(),
        desc="Flip BSP right"),
    
    # Switch to next/prev
    Key([mod], "Left",
        lazy.screen.prev_group(),
        desc="Switch to previous group"),
    
    Key([mod], "Right",
        lazy.screen.next_group(),
        desc="Switch to next group"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
    # ATM for BSP and Columns
    Key([mod, "shift"], "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab",
        lazy.next_layout(),
        desc="Toggle between layouts"),
    
    Key([mod, "shift"], "Tab",
        lazy.prev_layout(),
        desc="Toggle between layouts (backward)"),

    # Window control
    Key([mod], "w",
        lazy.window.kill(),
        desc="Kill focused window"),
    
    Key([mod], "t",
        lazy.window.toggle_floating(),
        desc="Toggle floating for focused window"),

    # Qtile utils
    Key([mod, "control"], "b",
        lazy.hide_show_bar("top"),
        desc="Toggle bar"),
    
    Key([mod, "control"], "r",
        lazy.reload_config(),
        desc="Reload the config"),
    
    Key([mod, "shift"], "r",
        lazy.restart(),
        desc="Restart Qtile"),
    
    Key([mod, "control"], "q",
        lazy.shutdown(),
        desc="Shutdown Qtile"),

    ################
    ##### XF86 #####
    ################

    # Volume
    Key([], "XF86AudioRaiseVolume",
        lazy.spawn(volume + " up"),
        desc="Volume up"),
    
    Key([], "XF86AudioLowerVolume",
        lazy.spawn(volume + " down"),
        desc="Volume down"),
    
    Key([], "XF86AudioMute",
        lazy.spawn(volume + " mute"),
        desc="Volume toggle mute"),

    ###################
    ##### GENERAL #####
    ###################
    
    ##### Rofi #####
    Key([mod], "d",
        lazy.spawn(launcher),
        desc="Rofi launcher"),
    
    Key([mod, "shift"], "q",
        lazy.spawn(powermenu),
        desc="Rofi powermenu"),

    Key([mod], "comma",
        lazy.spawn(clipboard + " copy"),
        desc="Rofi clipboard"),
    
    Key([mod], "period",
        lazy.spawn(clipboard + " paste"),
        desc="Rofi clipboard direct paste"),

    Key([mod], "slash",
        lazy.spawn(emoji),
        desc="Rofi emoji"),

    Key([mod], "p",
        lazy.spawn("rofi-rbw"),
        desc="Rofi RBW"),

    ##### FLAMESHOT #####

    # Full capture
    Key([], "Print",
        lazy.spawn("flameshot full -c"),
        desc="Full screenshot"),

    # Open GUI
    Key([mod], "Print",
        lazy.spawn("flameshot gui"),
        desc="Capture GUI"),

    ##### APPS #####
    
    # Terminal
    Key([mod], "Return",
        lazy.spawn(terminal),
        desc="Launch terminal"),

    # File Manager
    Key([mod], "f",
        lazy.spawn(filemanager),
        desc="Launch file manager"),

    # Browser
    Key([mod], "b",
        lazy.spawn(browser),
        desc="Launch browser"),

    # (E)macs
    KeyChord([mod], "e", [
        
        Key([], "e",
            lazy.spawn(editor),
            desc="Launch Emacs"),

        Key([], "m",
            lazy.spawn(mail),
            desc="Launch mail client"),

        # Key([], "g",
        #     lazy.spawn(git),
        #     desc="Launch Git client"),
    ]),

    # Global (R)un
    KeyChord([mod], "r", [

        Key([], "t",
            lazy.spawn("telegram-desktop"),
            desc="Launch Telegram"),

        Key([], "d",
            lazy.spawn("com.discordapp.Discord"),
            desc="Launch Discord"),

        Key([], "c",
            lazy.spawn("code"),
            desc="Launch VSCode"),

        Key([], "g",
            lazy.spawn(gitgui),
            desc="Launch Git client (GUI)"),

        Key([], "p",
            lazy.spawn(pdfview),
            desc="Launch PDF Viewer"),

        Key([], "s",
            lazy.spawn("env LD_PRELOAD=/usr/lib/spotify-adblock.so spotify %U"),
            desc="Launch Spotify (with ADB)"),
    ]),

    # Sysmon
    KeyChord([mod], "s", [

        Key([], "b",
            lazy.spawn(terminal + " -e btop")),

        Key([], "h",
            lazy.spawn(terminal + " -e htop")),
    ]) 
]

##### MOUSE #####

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]


############################
##### GROUPS + LAYOUTS #####
############################

##### GROUPS #####

groups = [
    Group("1", label="一", layout="monadtall"),
    Group("2", label="二", layout="columns"  ),
    Group("3", label="三", layout="monadtall"),
    Group("4", label="四", layout="monadtall"),
    Group("5", label="五", layout="monadtall"),
    Group("6", label="六", layout="monadtall"),
    Group("7", label="七", layout="monadtall"),
    Group("8", label="八", layout="monadtall"),
    Group("9", label="九", layout="monadtall"),
]

for i in groups:
    keys.extend([
        # Switch to group
         Key([mod], i.name, lazy.group[i.name].toscreen(),
             desc="Switch to group {}".format(i.name)),

         # Switch to & move focused window to group
         Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
             desc="Switch to & move focused window to group {}".format(i.name)),
    ])

##### LAYOUT MAIN #####
    
layout_theme = {
    "border_width": 3,
    "border_focus": colors["active"],
    "border_normal": colors["inactive"],
}  

layouts = [
    layout.MonadTall(
        **layout_theme,
        margin = 10,
    ),
    layout.Columns(
        **layout_theme,
        margin = 10,
        grow_amount = 5,
        num_columns = 3,
    ),
    layout.Bsp(
        **layout_theme,
        margin = 10,
        ratio = 1.5,
    ),
    layout.Max(),
    layout.Floating(
        **layout_theme,
    ),
]

##### FLOAT RULES #####

floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class='Thunar'),
        Match(wm_class='confirmreset'), 
        Match(wm_class='bitwarden'),  
        Match(wm_class='makebranch'), 
        Match(wm_class='maketag'),  
        Match(wm_class='ssh-askpass'),
        Match(title="Android Emulator.*"),
        Match(title='branchdialog'),  
        Match(title='pinentry'),  
        Match(wm_class='pinentry-gtk-2'),  
    ],
    **layout_theme,
)

##################
##### SCREEN #####
##################

##### CALLBACKS FUNCT #####

def open_mixer():
    qtile.cmd_spawn(mixer)

##### BASE ELEMENTS #####

widget_defaults = dict(
    font="JetBrainsMono Nerd Font",
    fontsize=20 if hostname == 'quietfrost' else 18,
    foreground=colors["fg"],
    padding=3,
)

def base(fg='fg', bg='bg'):
    return {
        'foreground': colors[fg],
        'background': colors[bg],
        'padding' : 8,
    }

def sep(fg='fg', bg='bg'):
     return widget.Sep(
         **base(fg, bg),
         linewidth = 0,
     )

##### WIDGETS FOR LAPTOP ##### 
 
def laptop_extra():
    global hostname
    if hostname == 'quietfrost':
        widgets_list = [
            sep(),
        ]
    else:
        widgets_list = [
            
            widget.Backlight(
                **base(fg='bg', bg='color1'),
                format=' {percent:2.0%}',
                backlight_name='amdgpu_bl0',
                brightness_file='brightness',
                max_brightness_file='max_brightness',
                change_command='brightnessctl s {0}%',
                step=5,
            ),
            
            widget.Battery(
                **base(fg='bg', bg='color1'),
                low_foreground=colors['urgent'],
                format='{char}{percent:2.0%}({hour:d}:{min:02d})',
                charge_char=' ',
                full_char='  ',
                discharge_char='  ',
                empty_char='  ',
                battery=1,
                low_percentage=0.15,
                notify_below=0.1,
            ),

            sep(),
        ]
    return widgets_list

##### BAR #####

screens= [
    Screen(
        top=bar.Bar(
            [
                ### LEFT 
                widget.GroupBox(
                    active=colors["fg"],
                    inactive=colors["inactive"],
                    highlight_method="block",
                    this_current_screen_border=colors["active"],
                    urgent_alert_method="text",
                    urgent_text=colors["urgent"],
                    disable_drag=True,
                    padding=8,
                ),
                
                widget.Spacer(),

                ### CENTER
                
                # widget.Mpris2(
                #     **base(fg='fg', bg='bg'),
                #     fmt="  {}",
                #     name="spotify",
                #     objname="org.mpris.MediaPlayer2.spotify",
                #     display_metadata=["xesam:title", "xesam:artist"],
                #     scroll_chars=15,
                #     scroll_interval=0.5,
                #     scroll_wait_intervals=8,
                # ),

                # widget.Spacer(),

                ### RIGHT

                widget.CurrentLayoutIcon(
                    **base(fg='bg', bg='fg'),
                    custom_icon_paths=[path.join(qtile_path, "icons")],
                    scale=0.7,
                ),

                sep(),
                
                widget.CheckUpdates(
                    **base(fg='bg', bg='color3'),
                    distro="Arch_checkupdates",
                    colour_have_updates=colors["bg"],
                    colour_no_updates=colors["bg"],
                    display_format=" {updates} updates",
                    no_update_string="  no updates",
                    update_interval=1800,
                    execute=update,
                ),

                sep(),

                widget.CPU(
                    **base(fg='bg', bg='color2'),
                    format="  {load_percent}%",
                    update_interval=5,
                ),
                
                sep(),
                
                widget.ThermalSensor(
                    **base(fg='bg', bg='urgent'),
                    tag_sensor='Tctl' if hostname == 'quietfrost' else None,
                    fmt=" {}",
                    treshold=75,
                    update_interval=5,
                ),

                sep(),

                widget.Memory(
                    **base(fg='bg', bg='color4'),
                    format="{MemUsed: .0f} MB",
                    update_interval=5,
                ),

                sep(),

                widget.PulseVolume(
                    **base(fg='bg', bg='color1'),
                    fmt=" {}",
                    limit_max_volume=True,
                    volume_app="pavucontrol",
                    mouse_callbacks={"Button2": open_mixer},
                ),

            ]
            +
            
            laptop_extra()

            +
            [
                widget.Clock(
                    **base(fg='bg', bg='active'),
                    format=" %R",
                ),

                sep(),
                
                widget.Systray(
                    **base(bg='inactive'),
                    icon_size=24,
                ),

                sep(bg='inactive'),
            ],
            size=34 if hostname == 'quietfrost' else 32,
            background=colors["bg"],
         ),
     ),
]

########################
##### LAST DETAILS #####
########################

##### FUNCTIONS #####

# Show/Hide bar based on layout
def _bar(qtile):
    # Get the bar 
    bar = qtile.current_screen.top
    # Check the layout and hide bar accordingly
    if(qtile.current_layout.info()['name'] == 'max'):
        bar.show(False)
    else:
        bar.show(True)

@hook.subscribe.layout_change
def layout_change(layout,group):
    _bar(qtile)
    
@hook.subscribe.changegroup
def group_change():
    _bar(qtile)

@hook.subscribe.client_focus
def focus_change(window):
    _bar(qtile)       

##### OTHER #####

extension_defaults = widget_defaults.copy()

main = None

dgroups_key_binder = None
dgroups_app_rules  = []

focus_on_window_activation = "smart"

follow_mouse_focus = True
bring_front_click  = False
cursor_warp        = False

reconfigure_screens = True

auto_minimize   = True
auto_fullscreen = True

wmname = "LG3D"
