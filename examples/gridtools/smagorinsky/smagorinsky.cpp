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

int main() {
  // Initialize fields
  auto repo = smagorinsky::repository(33, 28, 80);
  repo.init_fields();

  // Setup serializer
  serialbox::gridtools::serializer serializer(serialbox::gridtools::open_mode::Write,
                                              "./smagorinsky-stencil", "stencil");

  // Run and serialize stencil
  smagorinsky::run_stencil(repo, serializer, 1);

  return 0;
}
