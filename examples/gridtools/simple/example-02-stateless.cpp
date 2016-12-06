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
#define CXX11_ENABLED
#define STRUCTURED_GRIDS
#include <gridtools.hpp>
#include <stencil-composition/stencil-composition.hpp>

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
using storage_traits_t = gridtools::storage_traits<gridtools::enumtype::Host>;
using meta_data_t = storage_traits_t::meta_storage_type<0, gridtools::layout_map<0, 1, 2>>;
using storage_t = storage_traits_t::storage_type<double, meta_data_t>;

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
    meta_data_t meta_data(N, M, K);
    storage_t field_in(meta_data, "storage", -1);
    storage_t field_out(meta_data, "storage", -1);

    std::default_random_engine gen;
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    for(int i = 0; i < N; ++i)
      for(int j = 0; j < M; ++j)
        for(int k = 0; k < K; ++k)
          field_in(i, j, k) = dist(gen);

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
    for(int i = 0; i < N; ++i)
      for(int j = 0; j < M; ++j)
        for(int k = 0; k < K; ++k)
          if(field_in(i, j, k) != field_out(i, j, k))
            throw ser::exception("mismatch at (%i,%i) of in and out: %f vs. %f\n", i, j,
                                 field_in(i, j, k), field_out(i, j, k));

  } catch(std::exception& e) {
    std::cerr << "Errror: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}

