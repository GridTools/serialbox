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
#define FUSION_MAX_VECTOR_SIZE 20
#define FUSION_MAX_MAP_SIZE FUSION_MAX_VECTOR_SIZE
#define BOOST_MPL_LIMIT_VECTOR_SIZE FUSION_MAX_VECTOR_SIZE
#define BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS

#include <gridtools.hpp>
#include <stencil-composition/stencil-composition.hpp>
#include <stencil-composition/stencil-functions/stencil-functions.hpp>
#include <serialbox/gridtools/serialbox.hpp>

#include <string>
#include <random>

namespace smagorinsky {

// Fine-grained serialization is only supported by the Naive Host backend at the moment.
using backend = gridtools::backend<gridtools::enumtype::Host, gridtools::enumtype::structured,
                                   gridtools::enumtype::Naive>;

using storage_traits_t = gridtools::storage_traits<gridtools::enumtype::Host>;

/// @brief Axis of the Grid including the entire vertical domain
using axis_t = gridtools::interval<gridtools::level<0, -1>, gridtools::level<1, 1>>;

/// \brief All layers, meaning [0, k)
using full_domain_t = gridtools::interval<gridtools::level<0, -1>, gridtools::level<1, -1>>;

/// \brief Repository of the storages of the smagorinsky stencil
class repository {
public:
  static constexpr int halo_size = 3;

  /// \brief Floating point type
  using float_type = gridtools::float_type;

  /// \brief Horizontal halo
  using halo_ijk_t = gridtools::halo<3, 3, 0>;
  using halo_j_t = gridtools::halo<0, 3, 0>;

  /// \brief Storage-info types
  /// @{
  using storage_info_ijk_t = storage_traits_t::storage_info_t<0, 3, halo_ijk_t>;
  using storage_info_j_t =
      storage_traits_t::special_storage_info_t<1, gridtools::selector<0, 1, 0>, halo_j_t>;
  using storage_info_scalar_t =
      storage_traits_t::special_storage_info_t<2, gridtools::selector<0, 0, 0>>;
  /// @}

  /// \brief Storage types
  /// @{
  using storage_ijk_t = storage_traits_t::data_store_t<float_type, storage_info_ijk_t>;
  using storage_j_t = storage_traits_t::data_store_t<float_type, storage_info_j_t>;
  using storage_scalar_t = storage_traits_t::data_store_t<float_type, storage_info_scalar_t>;

  /// \brief Allocate the storages
  repository(int i, int j, int k)
      : isize_(i + 2 * halo_size), jsize_(j + 2 * halo_size), ksize_(k), //

        storage_info_scalar_(1, 1, 1), storage_info_j_(1, jsize_, 1),
        storage_info_ijk_(isize_, jsize_, ksize_),

        // Output fields
        u_out_(storage_info_ijk_, -1.0, "u_out"), v_out_(storage_info_ijk_, -1.0, "v_out"),

        // Input fields
        u_in_(storage_info_ijk_, -1.0, "u_in"), v_in_(storage_info_ijk_, -1.0, "v_in"),
        hdmaskvel_(storage_info_ijk_, -1.0, "hdmaskvel"),

        crlavo_(storage_info_j_, -1.0, "crlavo"), crlavu_(storage_info_j_, -1.0, "crlavu"),
        crlato_(storage_info_j_, -1.0, "crlato"), crlatu_(storage_info_j_, -1.0, "crlatu"),
        acrlat0_(storage_info_j_, -1.0, "acrlat0"),

        // Scalar fields
        eddlon_(storage_info_scalar_, -1.0, "eddlon"),
        eddlat_(storage_info_scalar_, -1.0, "eddlat"),
        tau_smag_(storage_info_scalar_, -1.0, "tau_smag"),
        weight_smag_(storage_info_scalar_, -1.0, "weight_smag") {}

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

    std::cout << "Initializing storages ..." << std::endl;

    std::default_random_engine generator;
    std::uniform_real_distribution<double> distribution(0.0, 1.0);

    auto u_out_view = make_host_view(u_out_);
    auto v_out_view = make_host_view(v_out_);
    auto u_in_view = make_host_view(u_in_);
    auto v_in_view = make_host_view(v_in_);
    auto hdmaskvel_view = make_host_view(hdmaskvel_);

    auto crlavo_view = make_host_view(crlavo_);
    auto crlavu_view = make_host_view(crlavu_);
    auto crlato_view = make_host_view(crlato_);
    auto crlatu_view = make_host_view(crlatu_);
    auto acrlat0_view = make_host_view(acrlat0_);

    auto eddlon_view = make_host_view(eddlon_);
    auto eddlat_view = make_host_view(eddlat_);
    auto tau_smag_view = make_host_view(tau_smag_);
    auto weight_smag_view = make_host_view(weight_smag_);

    for(int i = 0; i < isize_; i++)
      for(int j = 0; j < jsize_; j++)
        for(int k = 0; k < ksize_; k++) {
          u_out_view(i, j, k) = distribution(generator);
          v_out_view(i, j, k) = distribution(generator);
          u_in_view(i, j, k) = distribution(generator);
          v_in_view(i, j, k) = distribution(generator);
          hdmaskvel_view(i, j, k) = distribution(generator);
        }

    for(int j = 0; j < jsize_; j++) {
      crlavo_view(0, j, 0) = distribution(generator);
      crlavu_view(0, j, 0) = distribution(generator);
      crlato_view(0, j, 0) = distribution(generator);
      crlatu_view(0, j, 0) = distribution(generator);
      acrlat0_view(0, j, 0) = distribution(generator);
    }

    eddlon_view(0, 0, 0) = distribution(generator);
    eddlat_view(0, 0, 0) = distribution(generator);
    tau_smag_view(0, 0, 0) = distribution(generator);
    weight_smag_view(0, 0, 0) = distribution(generator);
  }

private:
  gridtools::uint_t isize_, jsize_, ksize_;

  // Meta-data
  storage_info_scalar_t storage_info_scalar_;
  storage_info_j_t storage_info_j_;
  storage_info_ijk_t storage_info_ijk_;

  // Output fields
  storage_ijk_t u_out_, v_out_;

  // Input fields
  storage_ijk_t u_in_, v_in_, hdmaskvel_;
  storage_j_t crlavo_, crlavu_, crlato_, crlatu_, acrlat0_;

  // Scalar fields
  storage_scalar_t eddlon_, eddlat_, tau_smag_, weight_smag_;
};

} // smagorinsky
