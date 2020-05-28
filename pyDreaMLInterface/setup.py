import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
     name='dreamlInterface',  
     version='0.1',
     scripts=['dreamlInterface'] ,
     author="Nitesh Kumar",
     author_email="niteshroyal.30@gmail.com",
     description="Multi-relational learning",
     long_description=long_description,
   long_description_content_type="text/markdown",
     url="https://github.com/niteshroyal/DreaML",
     packages=setuptools.find_packages(),
     classifiers=[
         "Programming Language :: Python :: 3",
         "License :: OSI Approved :: MIT License",
         "Operating System :: OS Independent",
     ],
 )
