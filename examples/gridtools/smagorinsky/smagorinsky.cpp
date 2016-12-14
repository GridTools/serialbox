//===-- smagorinsky.cpp -------------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file calls the Smagorinsky horizontal-diffusion stencil with serialized data and uses the
/// fine-grained deserialization of gridtools for further preprocessing with sdb.
///
//===------------------------------------------------------------------------------------------===//

#include "smagorinsky_repository.hpp"
#include "smagorinsky_stencil.hpp"

#include <serialbox/gridtools/serialbox.hpp>
#include <iostream>

namespace {
#ifdef ERROR
const char* output_dir = "./smagorinsky-stencil-error";
#else
const char* output_dir = "./smagorinsky-stencil";
#endif
}

int main() {
  //
  // Allocate & initialize fields
  //

  auto repo = smagorinsky::repository(33, 28, 80);
  repo.init_fields();

  try {

    //
    // Setup serializer
    //
    serialbox::gridtools::serializer serializer(serialbox::gridtools::open_mode::Write, output_dir,
                                                "stencil");

    //
    // Run & serialize smagorinsky stencil
    //

    smagorinsky::run_stencil(repo, serializer, 3);

  } catch(std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }

  std::cout << "Successfully ran Smagorinsky stencil!" << std::endl;
  return 0;
}
