all :
	bapbuild -clean
	bapbuild -package asli.libASL asli.plugin
	bapbundle install asli.plugin

clean :
	bapbuild -clean

uninstall:
	bapbundle remove asli.plugin
