all: dosubst.dll
dosubst.dll: dosubst.o
	i586-mingw32msvc-gcc -L/home/andrew/.wine/drive_c/Program\ Files/NSIS/Examples/Plugin/nsis/ -shared -o dosubst.dll dosubst.o -lpluginapi
dosubst.o: dosubst.c
	i586-mingw32msvc-gcc -I/home/andrew/.wine/drive_c/Program\ Files/NSIS/Examples/Plugin -c dosubst.c
