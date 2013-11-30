gnome-settings-daemon &
cinnamon-settings-daemon &
nm-applet &
dropbox start &
cinnamon-sound-applet &
eval `gnome-keyring-daemon`
setxkbmap -option "ctrl:swapcaps"
exec awesome