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
/// This example demonstrates how to use stateless serializations i.e serialize fields without the
/// need to register fields or savpoints. In addition, the usage of the logging infrastructure
/// is exemplified.
///
/// This example is also available in all other languages supported by Serialbox.
//
//===------------------------------------------------------------------------------------------===//

//
// Include gridtools headers (Serialbox only supports C++11)
//
#include <gridtools/stencil_composition/stencil_composition.hpp>

//
// Include Serialbox headers
//
#include "serialbox/gridtools/serialbox.hpp"

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
  int N = 10, M = 15, K = 20;
  try {

    //
    // The gridtools frontend of Serialbox is confined in serialbox::gridtools
    //
    namespace ser = serialbox::gridtools;

    //
    // Enable logging, by default it is turned off
    //
    ser::logging::enable();

    //
    // Allocate 3D arrays and fill the input with some random numbers
    //
    storage_info_t storage_info(N, M, K);

    std::default_random_engine gen;
    std::uniform_real_distribution<double> dist(0.0, 1.0);

    storage_t field_in(storage_info, [&](int i, int j, int k) { return dist(gen); }, "storage");
    storage_t field_out(storage_info, -1., "storage");

    //
    // Write the gridtools storage to disk. The archive will be deduced from the file extension
    // (here ".dat" implies the Binary archive). The written data can know also be read in C, Python
    // and Fortran.
    //
    ser::serializer::to_file("field.dat", field_in);

    //
    // Read the written field from file (note that this method performs no consistency checks so you
    // have to know what you are doing!).
    //
    ser::serializer::from_file("field.dat", field_out);

    //
    // Verify the result
    //
    {
      auto in = make_host_view(field_in);
      auto out = make_host_view(field_out);
      for(int i = 0; i < N; ++i)
        for(int j = 0; j < M; ++j)
          for(int k = 0; k < K; ++k)
            if(in(i, j, k) != out(i, j, k))
              throw ser::exception("mismatch at (%i,%i) of in and out: %f vs. %f\n", i, j,
                                   in(i, j, k), out(i, j, k));
    }
  } catch(std::exception& e) {
    std::cerr << "Errror: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
