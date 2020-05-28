# DreaMLInterface
* This interface allows other softwares to call DreaML. 

Installation
============
```
1. Install DreaML following the steps in the file : ../README.md

1. Build PyDC executable file and copy to the 'core' folder
   $ cd yapInterfaceForPython3
   $ python3 setup.py build_ext --inplace
   $ mv yapWrapper.cpython-35m-x86_64-linux-gnu.so ../yapWrapper.so
```
   	
Execution 
=========

1. An example of calling DreaML is shown in the file: DreaMLInterface.py

```
   $ python3 DreaMLInterface.py

```
