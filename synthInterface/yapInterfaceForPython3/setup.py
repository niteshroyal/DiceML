from distutils.core import setup, Extension
from Cython.Build import cythonize


setup(ext_modules = cythonize(Extension(
           "yapWrapper",                                # the extension name
           sources=["src/yapWrapper.pyx", "src/dc.cpp"], # the Cython source and
                                                  # additional C++ source files
           language="c++",                        # generate and compile C++ code
           include_dirs = ["include"],
           extra_compile_args = ["-std=c++11"],
           libraries = ["Yap"]
      )))
