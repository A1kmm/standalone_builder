all: selfinst

selfinst: start.o main.o libfuncs.o xz_crc32.o xz_dec_bcj.o xz_dec_lzma2.c xz_dec_stream.c
	gcc -Os -ggdb -Wall -nostdlib -m32 $+ -o selfinst
	cp selfinst selfinst.dbg
	strip -sx -R .note.gnu.build-id -R .eh_frame_hdr -R .eh_frame -R .comment selfinst

%.o: %.c
	gcc ${CFLAGS} -ggdb -Wall -U_FORTIFY_SOURCE -Os -Wno-unused-result -nostdlib -m32 -c $< -o $@
%.o: %.s
	as -32 $< -o $@
