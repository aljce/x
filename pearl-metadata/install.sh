post_install() {
	info "Installing Kyle McKean's x config"
	mkdir -p "$HOME/.xmonad"
	ln -sf "$PEARL_HOME/packages/default/x/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs" 
	ln -sf "$PEARL_HOME/packages/default/x/xmobar/.xmobarrc" "$HOME/.xmobarrc"
	ln -sf "$PEARL_HOME/packages/default/x/resources/.xinitrc" "$HOME/.xinitrc"
	ln -sf "$PEARL_HOME/packages/default/x/resources/.Xresources" "$HOME/.Xresources"
}

post_update() {
	post_remove
	post_install
}

post_remove() {
	info "Removing Kyle McKean's x config"
	rm -rf "$HOME/.xmonad"
	rm "$HOME/.xmobarrc"
	rm "$HOME/.xinitrc"
	rm "$HOME/.Xresources"
}


