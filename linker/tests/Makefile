all: platform

platform: platform.cc libapp.so
	$(CXX) -O2 -fPIC -fPIE -o $@ $^

libapp.so: app.cc
	$(CXX) -O2 -fPIC -shared -o $@ $^

clean:
	rm -f platform libapp.so
