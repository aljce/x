post_install() {
	info "Installing Kyle McKean's x config"
	mkdir -p "$HOME/.xmonad"
	ln -s "$PEARL_PKGDIR/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs" 
	ln -s "$PEARL_PKGDIR/xmobar/.xmobarrc" "$HOME/.xmobarrc"
	ln -s "$PEARL_PKGDIR/resources/.xinitrc" "$HOME/.xinitrc"
	ln -s "$PEARL_PKGDIR/resources/.Xresources" "$HOME/.Xresources"
	ln -s "$PEARL_PKGDIR/backgrounds/selected" "$HOME/.wallpaper"
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
	rm "$HOME/.wallpaper"
}


