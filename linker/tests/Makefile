SHLIB_LDFLAGS = -shared -Wl,--allow-shlib-undefined

all: platform

platform: platform.c libapp.so
	$(CC) -O2 -fPIC -o $@ $^

libapp.so: app.c
	$(CC) -O2 -fPIC -shared -o $@ $^

clean:
	rm -f platform libapp.so