# fortran-yaml-c

This is [YAML](http://yaml.org) parser for Fortran matching the YAML 1.1 spec.

This package uses the C package [libyaml](https://github.com/yaml/libyaml) to parse yaml documents, then stores the data in Fortran derived types created by [fortran-yaml](https://github.com/BoldingBruggeman/fortran-yaml). Hence the name `fortran-yaml-c`.

## Building

First, clone this repository

```
git clone --recursive https://github.com/Nicholaswogan/fortran-yaml-c.git
```

Next, navigate to the root directory of `fortran-yaml-c`. Finally, run the following commands to build with `cmake` and run the test and example.

```
mkdir build
cd build
cmake ..
cmake --build .
./test_yaml
./example
```