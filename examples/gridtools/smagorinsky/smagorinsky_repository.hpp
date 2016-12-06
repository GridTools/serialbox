//===-- smagorinsky_repository.hpp --------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Allocate and initialize the gridtools storages with serialized data.
///
//===------------------------------------------------------------------------------------------===//

#pragma once

#define CXX11_ENABLED
#define STRUCTURED_GRIDS
#include <gridtools.hpp>
#include <stencil-composition/stencil-composition.hpp>
#include <stencil-composition/structured_grids/call_interfaces.hpp>

#include <string>
#include <random>

namespace smagorinsky {

// Fine-grained serialization is only supported by the Naive Host backend at the moment.
using backend = gridtools::backend<gridtools::enumtype::Host, gridtools::enumtype::structured,
                                   gridtools::enumtype::Naive>;

/// @brief Axis of the Grid including the entire vertical domain
using axis_t = gridtools::interval<gridtools::level<0, -1>, gridtools::level<2, 3>>;

/// \brief All layers, meaning [0, k)
using full_domain_t = gridtools::interval<gridtools::level<0, -1>, gridtools::level<2, -1>>;

/// \brief Repository of the storages of the smagorinsky stencil
class repository {
public:
  static constexpr int halo_size = 3;

  /// \brief Floating point type
  using float_type = gridtools::float_type;

  /// \brief Horizontal halo
  using halo_ijk_t = gridtools::halo<3, 3, 0>;
  using halo_j_t = gridtools::halo<0, 3, 0>;

  /// \brief Layout types
  /// @{
  using layout_ijk_t = gridtools::layout_map<0, 1, 2>;
  using layout_j_t = gridtools::layout_map<-1, 0, -1>;
  using layout_scalar_t = gridtools::layout_map<-1, -1, -1>;
  /// @}

  /// \brief Meta-data types
  /// @{
  using metadata_ijk_t = backend::storage_info<0, layout_ijk_t, halo_ijk_t>;
  using metadata_ijk_tmp_t = backend::storage_info<1, layout_ijk_t, halo_ijk_t>;
  using metadata_j_t = backend::storage_info<2, layout_j_t, halo_j_t>;
  using metadata_scalar_t = backend::storage_info<3, layout_scalar_t>;
  /// @}

  /// \brief Storage types
  /// @{
  using storage_ijk_t = backend::storage_type<float_type, metadata_ijk_t>::type;
  using storage_ijk_tmp_t = backend::temporary_storage_type<float_type, metadata_ijk_tmp_t>::type;
  using storage_j_t = backend::storage_type<float_type, metadata_j_t>::type;
  using storage_scalar_t = backend::storage_type<float_type, metadata_scalar_t>::type;

  /// \brief Allocate the storages
  repository(int i, int j, int k)
      : isize_(i + 2 * halo_size), jsize_(j + 2 * halo_size), ksize_(k + 2 * halo_size),

        metadata_ijk_(isize_, jsize_, ksize_), metadata_j_(1, jsize_, 1), metadata_scalar_(1, 1, 1),

        // Output fields
        u_out_(metadata_ijk_, -1.0, "u_out"), v_out_(metadata_ijk_, -1.0, "v_out"),

        // Input fields
        u_in_(metadata_ijk_, -1.0, "u_in"), v_in_(metadata_ijk_, -1.0, "v_in"),
        hdmaskvel_(metadata_ijk_, -1.0, "hdmaskvel"),

        crlavo_(metadata_j_, -1.0, "crlavo"), crlavu_(metadata_j_, -1.0, "crlavu"),
        crlato_(metadata_j_, -1.0, "crlato"), crlatu_(metadata_j_, -1.0, "crlatu"),
        acrlat0_(metadata_j_, -1.0, "acrlat0"),

        // Scalar fields
        eddlon_(metadata_scalar_, -1.0, "eddlon"), eddlat_(metadata_scalar_, -1.0, "eddlat"),
        tau_smag_(metadata_scalar_, -1.0, "tau_smag"),
        weight_smag_(metadata_scalar_, -1.0, "weight_smag") {}

  /// \brief Getter
  /// @{
  storage_ijk_t& u_out() { return u_out_; }
  storage_ijk_t& v_out() { return v_out_; }

  storage_ijk_t& u_in() { return u_in_; }
  storage_ijk_t& v_in() { return v_in_; }
  storage_ijk_t& hdmaskvel() { return hdmaskvel_; }

  storage_j_t& crlavo() { return crlavo_; }
  storage_j_t& crlavu() { return crlavu_; }
  storage_j_t& crlato() { return crlato_; }
  storage_j_t& crlatu() { return crlatu_; }
  storage_j_t& acrlat0() { return acrlat0_; }

  storage_scalar_t& eddlon() { return eddlon_; }
  storage_scalar_t& eddlat() { return eddlat_; }
  storage_scalar_t& tau_smag() { return tau_smag_; }
  storage_scalar_t& weight_smag() { return weight_smag_; }
  /// @}

  gridtools::uint_t isize() const { return isize_; }
  gridtools::uint_t jsize() const { return jsize_; }
  gridtools::uint_t ksize() const { return ksize_; }

  /// \brief Load input data
  void init_fields() {

    std::default_random_engine generator;
    std::uniform_real_distribution<double> distribution(0.0, 1.0);

    for(int i = 0; i < isize_; i++)
      for(int j = 0; j < jsize_; j++)
        for(int k = 0; k < ksize_; k++) {

          u_out_(i, j, k) = distribution(generator);
          v_out_(i, j, k) = distribution(generator);
          u_in_(i, j, k) = distribution(generator);
          v_in_(i, j, k) = distribution(generator);
          hdmaskvel_(i, j, k) = distribution(generator);

          crlavo_(0, j, 0) = distribution(generator);
          crlavu_(0, j, 0) = distribution(generator);
          crlato_(0, j, 0) = distribution(generator);
          crlatu_(0, j, 0) = distribution(generator);
          acrlat0_(0, j, 0) = distribution(generator);
        }

    eddlon_(0, 0, 0) = distribution(generator);
    eddlat_(0, 0, 0) = distribution(generator);
    tau_smag_(0, 0, 0) = distribution(generator);
    weight_smag_(0, 0, 0) = distribution(generator);
  }

private:
  gridtools::uint_t isize_, jsize_, ksize_;
  metadata_ijk_t metadata_ijk_;
  metadata_j_t metadata_j_;
  metadata_scalar_t metadata_scalar_;

  // Output fields
  storage_ijk_t u_out_, v_out_;

  // Input fields)
  storage_ijk_t u_in_, v_in_, hdmaskvel_;
  storage_j_t crlavo_, crlavu_, crlato_, crlatu_, acrlat0_;

  // Scalar fields
  storage_scalar_t eddlon_, eddlat_, tau_smag_, weight_smag_;
};

} // smagorinsky
