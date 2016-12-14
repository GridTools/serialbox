//===-- smagorinsky_stencil.hpp -----------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Define Smagorinsky horizontal diffusion stencil.
///
//===------------------------------------------------------------------------------------------===//

#pragma once

#include "smagorinsky_repository.hpp"
#include <cmath>

namespace smagorinsky {

using namespace gridtools;
using namespace gridtools::enumtype;
using namespace gridtools::expressions;

constexpr static gridtools::dimension<1> i;
constexpr static gridtools::dimension<2> j;
constexpr static gridtools::dimension<3> k;

namespace math {
using std::min;
using std::max;
using std::sqrt;
}

//===------------------------------------------------------------------------------------------===//
//     Stage 1 - TensionShearStage
//===------------------------------------------------------------------------------------------===//

/// \brief Compute the tension and shear used for the Smagorinsky diffusion
struct tension_shear_stage {
  using T_sqr_s = inout_accessor<0>;
  using S_sqr_uv = inout_accessor<1>;

  using acrlat0 = in_accessor<2>;
  using eddlon = in_accessor<3>;
  using eddlat = in_accessor<4>;
  using u_in = in_accessor<5, extent<-1, 0, 0, 1>>;
  using v_in = in_accessor<6, extent<0, 1, -1, 0>>;

  using arg_list = boost::mpl::vector<T_sqr_s, S_sqr_uv, acrlat0, eddlon, eddlat, u_in, v_in>;

  template <typename Evaluation>
  GT_FUNCTION static void Do(const Evaluation& eval, full_domain_t) {
    const float_type frac_1_dx = eval(acrlat0(i, j, k) * eddlon(i, j, k));
    const float_type frac_1_dy = eval(eddlat(i, j, k)) / float_type(6371.229e3);

    // Tension
    const float_type T_s = eval(v_in(i, j - 1, k) - v_in(i, j, k)) * frac_1_dy -
                           eval(u_in(i - 1, j, k) - u_in(i, j, k)) * frac_1_dx;
    eval(T_sqr_s()) = T_s * T_s;

    // Shear
    const float_type S_uv = eval(u_in(i, j + 1, k) - u_in(i, j, k)) * frac_1_dy +
                            eval(v_in(i + 1, j, k) - v_in(i, j, k)) * frac_1_dx;
    eval(S_sqr_uv()) = S_uv * S_uv;
  }
};

//===------------------------------------------------------------------------------------------===//
//     Stage 2 - SmagCoeffStage
//===------------------------------------------------------------------------------------------===//

/// \brief Stage computing the coefficients for the Smagorinsky diffusion
struct smag_coeff_stage {
  using smag_u = inout_accessor<0>;
  using smag_v = inout_accessor<1>;

  using T_sqr_s = in_accessor<2, extent<0, 1, 0, 1>>;
  using S_sqr_uv = in_accessor<3, extent<-1, 0, -1, 0>>;
  using hdmaskvel = in_accessor<4>;
  using tau_smag = in_accessor<5>;
  using weight_smag = in_accessor<6>;

  using arg_list =
      boost::mpl::vector<smag_u, smag_v, T_sqr_s, S_sqr_uv, hdmaskvel, tau_smag, weight_smag>;

  template <typename Evaluation>
  GT_FUNCTION static void Do(const Evaluation& eval, full_domain_t) {
    const float_type hdweight = eval(weight_smag(i, j, k) * hdmaskvel(i, j, k));

    // i-direction
    const float_type smag_u_ =
        eval(tau_smag(i, j, k)) *
            math::sqrt((float_type)0.5 * (eval(T_sqr_s(i, j, k) + T_sqr_s(i + 1, j, k) +
                                               S_sqr_uv(i, j, k) + S_sqr_uv(i, j - 1, k)))) -
        hdweight;

    eval(smag_u()) = math::min((float_type)0.5, math::max((float_type)0.0, smag_u_));

// j-direction
#ifdef ERROR
    const float_type smag_v_ =
        eval(tau_smag(i, j, k)) *
            math::sqrt((float_type)0.5 * (eval(T_sqr_s(i, j, k) + T_sqr_s(i, j + 1, k) +
                                               S_sqr_uv(i, j, k) + S_sqr_uv(i - 1, j, k)))) + // <--
        hdweight;
#else
    const float_type smag_v_ =
        eval(tau_smag(i, j, k)) *
            math::sqrt((float_type)0.5 * (eval(T_sqr_s(i, j, k) + T_sqr_s(i, j + 1, k) +
                                               S_sqr_uv(i, j, k) + S_sqr_uv(i - 1, j, k)))) -
        hdweight;
#endif

    eval(smag_v()) = math::min((float_type)0.5, math::max((float_type)0.0, smag_v_));
  }
};

//===------------------------------------------------------------------------------------------===//
//     Stage 3 - SmagUpdateStage
//===------------------------------------------------------------------------------------------===//

/// \brief Function computing the the Laplacian of an input array
struct laplacian {
  using lap = inout_accessor<0>;
  using in = in_accessor<1, extent<-1, 1, -1, 1>>;
  using crlato = in_accessor<2>;
  using crlatu = in_accessor<3>;

  using arg_list = boost::mpl::vector<lap, in, crlato, crlatu>;

  template <typename Evaluation>
  GT_FUNCTION static void Do(const Evaluation& eval, full_domain_t) {
    eval(lap()) = eval(in(i + 1, j, k)) + eval(in(i - 1, j, k)) -
                  (float_type)2.0 * eval(in(i, j, k)) +
                  eval(crlato(i, j) * (in(i, j + 1, k) - in(i, j, k))) +
                  eval(crlatu(i, j) * (in(i, j - 1, k) - in(i, j, k)));
  }
};

/// \brief Stage updating the horizontal velocities using the Smagorinsy coefficients
struct smag_update_stage {
  using u_out = inout_accessor<0>;
  using v_out = inout_accessor<1>;

  using u_in = in_accessor<2, extent<-1, 1, -1, 1>>;
  using v_in = in_accessor<3, extent<-1, 1, -1, 1>>;
  using smag_u = in_accessor<4>;
  using smag_v = in_accessor<5>;
  using crlato = in_accessor<6>;
  using crlatu = in_accessor<7>;
  using crlavo = in_accessor<8>;
  using crlavu = in_accessor<9>;

  using arg_list =
      boost::mpl::vector<u_out, v_out, u_in, v_in, smag_u, smag_v, crlato, crlatu, crlavo, crlavu>;

  template <typename Evaluation>
  GT_FUNCTION static void Do(const Evaluation& eval, full_domain_t) {
    const float_type lapu =
        call<laplacian, full_domain_t>::at<0, 0, 0>::with(eval, u_in(), crlato(), crlatu());

    const float_type lapv =
        call<laplacian, full_domain_t>::at<0, 0, 0>::with(eval, v_in(), crlavo(), crlavu());

    eval(u_out()) = eval(u_in(i, j, k)) + eval(smag_u(i, j, k)) * lapu;
    eval(v_out()) = eval(v_in(i, j, k)) - eval(smag_v(i, j, k)) * lapv;
  }
};

//===------------------------------------------------------------------------------------------===//
//     Stencil
//===------------------------------------------------------------------------------------------===//

/// \brief Smagorninsky stencil
template <class SerializerType>
void run_stencil(repository& repo, SerializerType& serializer, int invocation_count = 1) {

  //
  //    Argument list
  //

  // Temporaries
  using T_sqr_s = arg<0, repository::storage_ijk_tmp_t>;
  using S_sqr_uv = arg<1, repository::storage_ijk_tmp_t>;
  using smag_u = arg<2, repository::storage_ijk_tmp_t>;
  using smag_v = arg<3, repository::storage_ijk_tmp_t>;

  // Output fields
  using u_out = arg<4, repository::storage_ijk_t>;
  using v_out = arg<5, repository::storage_ijk_t>;

  // Input fields
  using u_in = arg<6, repository::storage_ijk_t>;
  using v_in = arg<7, repository::storage_ijk_t>;
  using hdmaskvel = arg<8, repository::storage_ijk_t>;
  using crlavo = arg<9, repository::storage_j_t>;
  using crlavu = arg<10, repository::storage_j_t>;
  using crlato = arg<11, repository::storage_j_t>;
  using crlatu = arg<12, repository::storage_j_t>;
  using acrlat0 = arg<13, repository::storage_j_t>;

  // Scalar fields
  using eddlon = arg<14, repository::storage_scalar_t>;
  using eddlat = arg<15, repository::storage_scalar_t>;
  using tau_smag = arg<16, repository::storage_scalar_t>;
  using weight_smag = arg<17, repository::storage_scalar_t>;

  using arg_list = boost::mpl::vector<
      // Temporaries
      T_sqr_s, S_sqr_uv, smag_u, smag_v,

      // Output fields
      u_out, v_out,

      // Input fields
      u_in, v_in, hdmaskvel, crlavo, crlavu, crlato, crlatu, acrlat0,

      // Scalar fields
      eddlon, eddlat, tau_smag, weight_smag>;

  //
  //    Domain
  //

  auto domain = aggregator_type<arg_list>(boost::fusion::make_vector(
      // Output fields
      &repo.u_out(), &repo.v_out(),

      // Input fields
      &repo.u_in(), &repo.v_in(), &repo.hdmaskvel(), &repo.crlavo(), &repo.crlavu(), &repo.crlato(),
      &repo.crlatu(), &repo.acrlat0(),

      // Scalar fields
      &repo.eddlon(), &repo.eddlat(), &repo.tau_smag(), &repo.weight_smag()));

  //
  //    Grid
  //

  const int halo_size = 3;

  // minus, plus, begin, end, length
  uint_t di[5] = {halo_size, halo_size, halo_size, repo.isize() - halo_size - 2,
                  repo.isize() - 2 * halo_size};
  uint_t dj[5] = {halo_size, halo_size, halo_size, repo.jsize() - halo_size - 2,
                  repo.jsize() - 2 * halo_size};

  gridtools::grid<axis_t> grid(di, dj);
  grid.value_list[0] = 0;
  grid.value_list[1] = repo.ksize() - 1;

  //
  //    Computation
  //

  auto computation = make_computation<backend>(
      domain, grid,
      make_multistage(execute<forward>(),

                      // 1. Stage - Tension shear
                      make_stage<tension_shear_stage>(T_sqr_s(), S_sqr_uv(), acrlat0(), eddlon(),
                                                      eddlat(), u_in(), v_in()),

                      // 2. Stage - Smagorinsky coefficient
                      make_stage<smag_coeff_stage>(smag_u(), smag_v(), T_sqr_s(), S_sqr_uv(),
                                                   hdmaskvel(), tau_smag(), weight_smag()),

                      // 3. Stage - Update output variable
                      make_stage<smag_update_stage>(u_out(), v_out(), u_in(), v_in(), smag_u(),
                                                    smag_v(), crlato(), crlatu(), crlavo(),
                                                    crlavu())));

  //
  // Run stencil
  //

  computation->ready();
  computation->steady();

  std::cout << "Running smagorinsky stencil ... " << std::endl;
  for(int i = 0; i < invocation_count; ++i) {
    std::cout << "Invocation: " << i << std::endl;
    computation->run(serializer, "Smagorinsky");
  }

  computation->finalize();
}

} // namespace smagorinsky
