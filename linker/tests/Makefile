all: platform

platform: platform.c libapp.so
	$(CC) -O2 -fPIC -fPIE -o $@ $^

libapp.so: app.c
	$(CC) -O2 -fPIC -shared -o $@ $^

clean:
	rm -f platform libapp.so