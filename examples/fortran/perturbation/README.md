# Serialbox Fortran example

This small example shows how to use Serialbox in a Fortran code using the serialization directives.

The serialization happens in the `m_ser.f90` module. Three small programs use this module to serialize and deserialize data.

It is important to use the same tool-chain (C++ and Fortran compiler) than the one used to build Serialbox.

### Build the example
```bash
mkdir build && cd build
cmake ../
make
```

### Run the example
```bash
./main_producer
./main_consumer
./main_consumer_perturb
```

or

```bash
bash run.sh
```

You should see something like:

```bash

Produce serialized data

 CALL serialize with sum(a)=   625.00000000000000
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<
 >>> WARNING: SERIALIZATION IS ON <<<
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<

Consume serialized data without pertubation

 Before read from serializer: sum(a)=   0.0000000000000000
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<
 >>> WARNING: SERIALIZATION IS ON <<<
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<
 After read from serializer: sum(a)=   625.00000000000000

Consume serialized data WITH pertubation

 Before read from serializer: sum(a)=   0.0000000000000000
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<
 >>> WARNING: SERIALIZATION IS ON <<<
 >>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<
 After read from serializer: sum(a)=   624.99993729204868

```
