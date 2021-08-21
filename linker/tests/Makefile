SHARED_ARGS = -fPIC -ffunction-sections

all: platform app.o

platform: platform.o linker.ld libapp.so
	$(CXX) $(SHARED_ARGS) -L. -lapp -fPIE -T linker.ld -o $@ $<

platform.o: platform.cc
	$(CXX) $(SHARED_ARGS) -c -o $@ $^

app.o: app.cc
	$(CXX) -fPIC -c -o $@ $^

libapp.so: app.cc
	$(CXX) $(SHARED_ARGS) -shared -o $@ $^

clean:
	rm -f platform platform.o libapp.so
