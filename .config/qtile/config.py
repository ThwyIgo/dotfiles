

# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Required packages for this configuration (Arch):
# alsa-utils playerctl rofi

import os
import subprocess

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, KeyChord
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()
home = os.path.expanduser('~')
file_manager = "pcmanfm"
web_browser = "vivaldi-stable"
email_client = "thunderbird"
volume_app = "pavucontrol"
    
keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
#    Key([mod], "space", lazy.layout.next(),
#        desc="Move window focus to other window"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Flip windows
    Key([mod, "mod1"], "j", lazy.layout.flip_down(),
        desc="Flip layout down"),
    Key([mod, "mod1"], "k", lazy.layout.flip_up(),
        desc="Flip layout up"),
    Key([mod, "mod1"], "h", lazy.layout.flip_left(),
        desc="Flip layout left"),
    Key([mod, "mod1"], "l", lazy.layout.flip_right(),
        desc="Flip layout right"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "t", lazy.spawn(terminal), desc="Launch terminal"),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "control"], "Return", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle window floating on/off"),
    Key([mod], "m", lazy.window.toggle_fullscreen(), desc="Toggle window full-screen on/off"),

    # Media controls
    Key([], "XF86AudioMicMute", lazy.spawn('amixer set Capture toggle && amixer get Capture | grep \'\[off\]\' && notify-send "MIC switched OFF" || notify-send "MIC switched ON"')),
    Key([], "XF86AudioMute", lazy.spawn('amixer set Master toggle')),
    Key([], "XF86AudioRaiseVolume", lazy.spawn('pactl set-sink-volume 0 +5%')),
    Key([], "XF86AudioLowerVolume", lazy.spawn('pactl set-sink-volume 0 -5%')),
    Key([mod, "shift"], "m", lazy.spawn('amixer set Master toggle')),
    Key([mod, "shift"], "Up", lazy.spawn('pactl set-sink-volume 0 +5%')),
    Key([mod, "shift"], "Down", lazy.spawn('pactl set-sink-volume 0 -5%')),
    Key([mod, "shift"], "Left", lazy.spawn('playerctl previous')),
    Key([mod, "shift"], "Right", lazy.spawn('playerctl next')),
    Key([mod, "shift"], "space", lazy.spawn('playerctl --player=spotify play-pause')),

    # Spawn apps
    Key([mod], "Return",
             lazy.spawn("rofi -show drun"),
             desc='rofi launcher'
             ),
    Key(["mod1"], "Tab", #Alt + Tab = mudar janela
        lazy.spawn("rofi -show window"),
        desc='rofi change window'
        ),
#    Key([mod], "period",
#        lazy.spawn('rofi -show emoji -modi emoji'),
#        desc='rofi emoji menu'
#        ),
    Key([mod], "d",
        lazy.spawn([home + '/.config/rofi/scripts-menu.sh']),
        desc='Launch scripts'
        ),
    Key([], "XF86Calculator", lazy.spawn('qalculate-gtk'),
        desc='Launch Qalculate!'),
    Key([], "XF86Mail", lazy.spawn(email_client),
        desc='Launch Email client'),
    KeyChord([mod],"e", [
        Key([], "e",
            lazy.spawn("env XMODIFIERS= emacsclient -c -a 'emacs'"),
            desc='Launch Emacs'
            ),
        Key([], "t",
            lazy.spawn("telegram-desktop"),
            desc='Launch Telegram'
            ),
        Key([], "b",
            lazy.spawn(web_browser),
            desc='Launch Web Browser'
            ),
        Key([], "f",
            lazy.spawn(file_manager),
            desc='Launch File Manager'
            ),
        Key([], "m",
            lazy.spawn(email_client),
            desc='Launch Email client'
            ),
        Key([], "v",
            lazy.spawn(volume_app),
            desc='Launch volume control app'
            ),
    ]),

    # Utils
    Key([], "Print",
             lazy.spawn([home + '/.config/qtile/scripts/screenshot-full.sh']),
             desc='Full screenshot'
             ),
    Key([mod], "Print",
             lazy.spawn([home + '/.config/qtile/scripts/screenshot-select.sh']),
             desc='Select screenshot'
             ),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    layout.Bsp(border_width=4, fair=False, margin=0),
    layout.Columns(border_focus_stack=['#d75f5f', '#8f3d3d'], border_width=4),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
    # layout.Slice(),
    # layout.Max(),
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        'launch': ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.Systray(),
                #widget.PulseVolume(foreground = "#dd8f08"),
                #widget.Volume(padding = 5, foreground = "#dd8f08", ),
                widget.TextBox("Arch btw", foreground="#d75f5f"),
                widget.Clock(format='%Y-%m-%d %a %H:%M'),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
    Match(title='Qalculate!'),        # qalculate-gtk
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
bring_front_click = "floating_only"

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])
@hook.subscribe.client_new
def client_new(client):
    if client.name == 'KeePassXC':
        client.togroup('5')
#Iniciar telgram em segundo plano. Precisa de ajustes para que a ação só funcione na primeira vez em que ele é invocado
# @hook.subscribe.client_new
# def client_new(client):
#     if client.name == 'Telegram':
#         client.togroup('3')
#         client.kill()
    
# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
