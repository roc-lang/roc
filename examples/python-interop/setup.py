import os
from setuptools import setup, Extension

def main():
    setup(name="demo",
          description="Demo testing Roc function calls from Python",
          ext_modules=[
              Extension("demo", sources=["demo.c"],
                        libraries=["hello"], library_dirs=[os.getcwd()])])

if __name__ == "__main__":
    main()
