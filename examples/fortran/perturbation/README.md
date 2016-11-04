# Serialbox Fortran example

This small example shows how to use Serialbox in a Fortran code using the
serialization directives.

The serialization happens in the `m_ser.f90` module. Two small programs use
this module to serialize and deserialize data.


It is important to use the same Fortran compiler than the one used to build 
Serialbox.

### Build the example
```bash
cmake .
make
```

### Run the example
```bash
./main_producer
./main_consumer
./main_consumer_perturb
```
