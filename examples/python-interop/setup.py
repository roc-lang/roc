import os
from distutils.core import setup, Extension

os.environ["LD_LIBRARY_PATH"] += ':' + os.getcwd()
print(os.environ["LD_LIBRARY_PATH"]);

def main():
    setup(name="demo",
          version="1.0.0",
          description="Python interface for the fputs C library function",
          author="dankey",
          ext_modules=[
              Extension("demo", sources=["demo.c"],
                        libraries=["hello"], library_dirs=["/home/dankey/dev/roc/examples/python-interop"]
                        )
          ])

if __name__ == "__main__":
    main()
