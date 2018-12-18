//===--------------------------------------------------------------------------------*- C++ -*-===//
//
//                                   S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information.
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This example demonstrates how to only load parts (slices) of a serialized field. This can
/// significantly improve performance if one is only interested in a small part of the data.
///
/// This example is also available in all other languages supported by Serialbox.
//
//===------------------------------------------------------------------------------------------===//

//
// Include gridtools headers (Serialbox only supports C++11)
//
#define CXX11_ENABLED
#define STRUCTURED_GRIDS
#include <gridtools/stencil-composition/stencil-composition.hpp>

//
// Include Serialbox headers
//
#include "serialbox/gridtools/serialbox.hpp"

#include <cstdio>
#include <exception>
#include <iostream>
#include <random>

//
// Typedefs of the gridtools library
//
using storage_traits_t = gridtools::storage_traits<gridtools::target::x86>;
using storage_info_t = storage_traits_t::storage_info_t<0, 3>;
using storage_t = storage_traits_t::data_store_t<double, storage_info_t>;

int main() {
  int N = 512, M = 512, K = 80;
  try {

    //
    // The gridtools frontend of Serialbox is confined in serialbox::gridtools
    //
    namespace ser = serialbox::gridtools;

    //
    // Initialize the serializer. At the moment sliced loading is only supported by the Binary
    // archive
    //
    ser::serializer serializer_write(ser::open_mode::Write, "./slice", "field", "Binary");

    //
    // Allocate 3D arrays and fill the input with some random numbers
    //
    storage_info_t storage_info(N, M, K);

    std::default_random_engine gen;
    std::uniform_real_distribution<double> dist(0.0, 1.0);

    storage_t field_in(storage_info, [&](int i, int j, int k) { return dist(gen); }, "storage");
    storage_t field_out(storage_info, -1., "storage");

    //
    // Write the gridtools storage to disk at Savepoint `sp`
    //
    ser::timer t;

    auto savepoint = ser::savepoint("sp");
    serializer_write.write("field", savepoint, field_in);

    std::printf("serializer.write       : %8.2f ms\n", t.stop());

    //
    // Initialize a serializer for reading.
    //
    ser::serializer serializer_read(ser::open_mode::Read, "./slice", "field", "Binary");

    //
    // Assume we are only interested in a certain layer of the data (k = 50), we can use the slice
    // object (ser::slice) to encode this information and instruct the serializer to only load
    // the desired data. Note that you still need to allocate memory for the whole field.
    // The syntax for slicing follows closely the slicing syntax used in Python, the equivalent of
    // `[start1:stop1:step1, ... ,startN:stopN:stepN]` is
    // `slice(start1, stop1, step1) ... (startN, stopN, stepN)`. In python we used:
    // `serializer_read.read_slice('field', savepoint, ser.Slice[:, :, 50], field_out)` which is
    // equivalent to ...
    //
    t.start();

    serializer_read.read_slice("field", savepoint, field_out, ser::slice()()(50, 51));

    std::printf("serializer.read_slice  : %8.2f ms\n", t.stop());

    //
    // Verify
    //
    {
      auto in = make_host_view(field_in);
      auto out = make_host_view(field_out);
      for(int i = 0; i < N; ++i)
        for(int j = 0; j < M; ++j)
          if(in(i, j, 50) != out(i, j, 50))
            throw ser::exception("mismatch at (%i,%i) of in and out: %f vs. %f\n", i, j,
                                 in(i, j, 50), out(i, j, 50));
    }

    //
    // For comparison, let's load the full data.
    //
    t.start();

    serializer_read.read("field", savepoint, field_out);

    std::printf("serializer.read        : %8.2f ms\n", t.stop());

    //
    // Verify
    //
    {
      auto in = make_host_view(field_in);
      auto out = make_host_view(field_out);
      for(int i = 0; i < N; ++i)
        for(int j = 0; j < M; ++j)
          if(in(i, j, 50) != out(i, j, 50))
            throw ser::exception("mismatch at (%i,%i) of in and out: %f vs. %f\n", i, j,
                                 in(i, j, 50), out(i, j, 50));
    }

    //
    // Remove directory
    //
    serialbox::remove_all("./slice");

  } catch(std::exception& e) {
    std::cerr << "Errror: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
