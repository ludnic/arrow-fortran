# arrow-fortran
Test automatic bindings to the C API for the arrow project. 

The interfaces have been generated with the [cfwrapper script](https://github.com/vmagnin/gtk-fortran/wiki/How-to-hack-the-cfwrapper) from the gtk-fortran project. 

To test the result we follow the examples in the [getting started page](https://arrow.apache.org/docs/cpp/tutorials/basic_arrow.html) of the arrow project and reproduce with the Fortran bindings the examples used there to demonstrate the basic usage. 

The basic c++ example is provided in the `cpp_example` folder along with a Makefile. The Fortran equivalent is implemented in main.f90. 

## Building the project 

The project includes a development environment which includes all the dependencies. Using VSCode and Docker it should be as simple as opening the project in a development container with the remote development extension.

Without using the included docker container linking against the arrow libraries requires installing the libraries, instructions are available for different environments on the [arrow installation page](https://arrow.apache.org/install/).