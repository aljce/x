set -e
echo "Installing Kyle McKean's x config"
mkdir -p "$HOME/.xmonad"
ln -sf "$PWD/x/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs" 
ln -sf "$PWD/x/xmobar/.xmobarrc" "$HOME/.xmobarrc"
ln -sf "$PWD/x/resources/.Xresources" "$HOME/.Xresources"
ln -sf "$PWD/x/backgrounds/selected" "$HOME/.wallpaper"
echo "Kyle McKeans's x config installed"
