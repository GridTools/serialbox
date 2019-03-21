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
/// This example demonstrates how to setup a Serializer, add global meta-information, register
/// fields and savepoints and serialize/deserialize gridtools storages using the gridtools frontend
/// of Serialbox.
///
/// In this small example we will repeatedly apply a two dimensional laplacian stencil to an input
/// field `phi`. Before and after each invocation of the laplacian stencil, we will serialize the
/// data to disk.
///
/// This example is also available in all other languages supported by Serialbox.
//
//===------------------------------------------------------------------------------------------===//

//
// Include gridtools headers (Serialbox only supports C++11)
//
#include <gridtools/stencil_composition/stencil_composition.hpp>
#include <gridtools/storage/storage_facility.hpp>

//
// Include Serialbox headers
//
#include "serialbox/gridtools/serialbox.hpp"

#include <exception>
#include <iostream>
#include <random>

static constexpr int N = 10;
static constexpr int M = 10;
static constexpr int halo_size = 1;

//
// Typedefs of the gridtools library
//
using storage_traits_t = gridtools::storage_traits<gridtools::target::x86>;
using backend_t = gridtools::backend<gridtools::target::x86, gridtools::grid_type::structured,
                                     gridtools::strategy::naive>;
using halo_t = gridtools::halo<halo_size, halo_size, 0>;
using storage_info_t =
    storage_traits_t::special_storage_info_t<0, gridtools::selector<1, 1, 0>, halo_t>;
using storage_t = storage_traits_t::data_store_t<double, storage_info_t>;

//
// Laplacian stage
//
struct laplacian_stage {
  using lap = gridtools::inout_accessor<0>;
  using phi = gridtools::in_accessor<1, gridtools::extent<-1, 1, -1, 1>>;

  using arg_list = boost::mpl::vector<lap, phi>;

  template <typename Evaluation>
  GT_FUNCTION static void Do(Evaluation& eval) {
    eval(lap()) =
        eval(phi(1, 0)) + eval(phi(-1, 0)) + eval(phi(0, -1)) + eval(phi(0, 1)) - 4 * eval(phi());
  }
};

//===------------------------------------------------------------------------------------------===//
//  write()
//
// In this function we first prepare the Serializer for writing, add some global meta-information
// and register the fields `phi` and `lap`. Later, we apply the `laplacianStencil` to `phi` and
// `lap` and serialize every iteration `phi` as an input and `lap` as an output of the stencil.
//
//===------------------------------------------------------------------------------------------===//
void write() {
  //
  // The gridtools frontend of Serialbox is confined in serialbox::gridtools
  //
  namespace ser = serialbox::gridtools;

  //
  // Create a Serializer for writing. Besides the open-policy, we have to specify the `directory`
  // in which the Serializer is created and the `prefix` of all files. In case the directory does
  // not exist, it will be created. In addition, if the directory is not empty, all fields with the
  // same `prefix` will be erased (this behaviour can be inhibited using the Append mode).
  //
  ser::serializer serializer(ser::open_mode::Write, "./laplacian/", "field");

  //
  // Allocate the 2D arrays phi and lap and fill it with some random numbers
  //
  storage_info_t storage_info(N, M, 1);

  std::default_random_engine gen;
  std::uniform_real_distribution<double> dist(0.0, 1.0);

  storage_t phi(storage_info, [&](int i, int j, int k) { return dist(gen); }, "phi");
  storage_t lap(storage_info, -1., "lap");

  //
  // Create the field meta-information of `phi` directly with the gridtools storage and register it
  // within the Serializer. For the gridtools interface this can also be done implicitly in the
  // write method (see below).
  //
  ser::field_meta_info fieldmetainfo(phi);
  serializer.register_field("phi", fieldmetainfo);

  //
  // Add some global meta-information to the serializer. Besides the usual `key = value` pair,
  // you can also add `key = {value1, ..., valueN}` pairs.
  //
  serializer.add_global_meta_info("answer", 42);
  serializer.add_global_meta_info("halos", std::vector<int>{1, 1, 1, 1});

  //
  // Up to this point nothing has been written to disk. Using update_meta_data() will force a write
  // of all meta-information to the corresponding JSON files. Note that the meta-data is updated
  // after each call and thus a manual update of the meta-data is seldom required. If you are
  // curious you can inspect the files './laplacian/MetaData-field.json' and
  // './laplacian/ArchiveMetaData-field.json'
  //
  serializer.update_meta_data();

  //
  // We now assemble the gridtools stencil
  //
  using p_lap = gridtools::arg<0, storage_t>;
  using p_phi = gridtools::arg<1, storage_t>;
  using arg_list = boost::mpl::vector<p_lap, p_phi>;

  // Setup grid
  gridtools::halo_descriptor di{halo_size, halo_size, halo_size, N - halo_size - 1, N};
  gridtools::halo_descriptor dj{halo_size, halo_size, halo_size, M - halo_size - 1, M};

  // Make computation
  auto laplacian_stencil = gridtools::make_computation<backend_t>(
      gridtools::make_grid(di, dj, 1), (p_lap() = lap), (p_phi() = phi),
      gridtools::make_multistage(gridtools::enumtype::execute<gridtools::enumtype::forward>(),
                                 gridtools::make_stage<laplacian_stage>(p_lap(), p_phi())));

  //
  // Now, we apply the `laplacian_stencil` three times to phi. In each iteration we will create
  // an input and output savepoint where we save the current `phi` field (input) and `lap` field
  // (output).
  //
  for(int t = 0; t < 3; ++t) {
    //
    // Create a Savepoint. Savepoints can have the same name as long as they have different
    // meta-information. In our case we will always store the current time step `t` as a
    // meta-information, thus making it unique.
    //
    ser::savepoint savepoint_in("laplacian-in");
    savepoint_in.meta_info().insert("time", t);

    //
    // Register the Savepoint.
    //
    serializer.register_savepoint(savepoint_in);

    //
    // Write phi to disk at our input savepoint. This will create the file `field_phi.dat` upon
    // first invocation and afterwards the data is appended.
    //
    serializer.write("phi", savepoint_in, phi);

    //
    // Apply the laplacian_stencil to phi
    //
    laplacian_stencil.run();

    //
    // Create the output savepoint. This time we directly initialize the meta-information of the
    // savepoint.
    //
    ser::savepoint savepoint_out("laplacian-out", {{"time", ser::meta_info_value(t)}});

    //
    // Write lap to disk. Note that here we implicitly register the field `lap` upon first
    // invocation. Same goes for the output savepoint.
    //
    serializer.write("lap", savepoint_out, lap);

    //
    // Finally, we swap phi with lap (usually you want to use a gridtools::data_field for this
    // task!)
    //
    auto lap_view = make_host_view(lap);
    auto phi_view = make_host_view(phi);
    for(int i = 0; i < N; ++i)
      for(int j = 0; j < M; ++j)
        std::swap(phi_view(i, j, 0), lap_view(i, j, 0));
  }
}

template <class VectorType>
std::string vec_to_string(VectorType&& vec) {
  std::stringstream ss;
  ss << "[ ";
  for(auto it = vec.begin(), end = vec.end(); it != end; ++it)
    ss << *it << " ";
  ss << "]";
  return ss.str();
}

//===------------------------------------------------------------------------------------------===//
//  read()
//
// In this function we initialize the Serializer for reading with our serialized data from the
// write() method. First, we query some meta-data, like the global meta-information, the
// dimensions of field `phi` or the vector of savepoints.
// Afterwards, we apply the same three time steps of the `laplacianStencil` to `phi` to compute
// `lap`. However, this time we compare the result (i.e the content of `lap`) to the reference
// loaded from disk (`lap_reference`) which we computed in the write() method. Obviously, the
// results will match as we apply the exact same stencil but in a real world scenario you might use
// a different implementations of the stencil and this is where Serialbox has it's use case.
//
//===------------------------------------------------------------------------------------------===//
void read() {
  namespace ser = serialbox::gridtools;

  //
  // Create a Serializer for reading. This gives access to the previously written data.
  //
  ser::serializer serializer(ser::open_mode::Read, "./laplacian/", "field");

  //
  // Access the global meta-information
  //
  std::cout << "The answer is " << serializer.global_meta_info().as<int>("answer") << std::endl;

  std::vector<int> halos = serializer.global_meta_info().as<std::vector<int>>("halos");
  std::cout << "The halo boundaries are " << vec_to_string(halos) << std::endl;

  //
  // Access the field meta-information
  //
  std::vector<std::string> fieldnames = serializer.fieldnames();
  std::cout << "The registered fields are: " << vec_to_string(fieldnames) << std::endl;

  const std::vector<int>& dims_of_phi = serializer.get_field_meta_info("phi").dims();
  std::cout << "Dimensions of phi: " << vec_to_string(dims_of_phi) << std::endl;

  //
  // Access the savepoints. The savepoints are ordered in the order they were inserted.
  //
  const std::vector<ser::savepoint>& savepoints = serializer.savepoints();
  std::cout << "Savepoints:\n";
  for(std::size_t i = 0; i < savepoints.size(); ++i)
    std::cout << " " << savepoints[i] << "\n";

  //
  // Allocate the 2D arrays phi and lap and assemble the gridtools stencil
  //
  storage_info_t meta_data(N, M, 1);
  storage_t phi(meta_data, -1., "phi");
  storage_t lap(meta_data, -1., "lap");
  storage_t lap_reference(meta_data, -1., "lap_reference");

  using p_lap = gridtools::arg<0, storage_t>;
  using p_phi = gridtools::arg<1, storage_t>;
  using arg_list = boost::mpl::vector<p_lap, p_phi>;

  // Setup grid
  gridtools::halo_descriptor di = {halo_size, halo_size, halo_size, N - halo_size - 1, N};
  gridtools::halo_descriptor dj = {halo_size, halo_size, halo_size, M - halo_size - 1, M};

  // Make computation
  auto laplacian_stencil = gridtools::make_computation<backend_t>(
      gridtools::make_grid(di, dj, 1), (p_lap() = lap), (p_phi() = phi),
      gridtools::make_multistage(gridtools::enumtype::execute<gridtools::enumtype::forward>(),
                                 gridtools::make_stage<laplacian_stage>(p_lap(), p_phi())));
  //
  // We will now perform the same iterations as in the write method but this time we will read
  // phi as an input from disk, compute the laplacian and compare the result to the stored output
  // of lap on disk (loaded as `lap_refrence`).
  //
  for(int t = 0; t < 3; ++t) {
    //
    // Get the current input savepoint at time t (the factor of 2 is due to the fact that we
    // stored input and output in alternating order).
    //
    const ser::savepoint& savepoint_in = savepoints[2 * t];

    //
    // Load phi from disk.
    //
    serializer.read("phi", savepoint_in, phi);

    //
    // Apply the laplacian_stencil to phi
    //
    laplacian_stencil.run();

    //
    // Load the refrence output of lap ...
    //
    const ser::savepoint& savepoint_out = savepoints[2 * t + 1];
    serializer.read("lap", savepoint_out, lap_reference);

    //
    // ... and compare the results.
    //
    auto lap_view = make_host_view(lap);
    auto lap_reference_view = make_host_view(lap_reference);
    for(int i = 1; i < N - 1; ++i)
      for(int j = 1; j < M - 1; ++j)
        if(lap_view(i, j, 0) != lap_reference_view(i, j, 0))
          throw ser::exception("mismatch at (%i,%i) of lap and lap_reference: %f vs. %f\n", i, j,
                               lap_view(i, j, 0), lap_reference_view(i, j, 0));
  }
}

//===------------------------------------------------------------------------------------------===//
//  main()
//
// Here we call our write() and read() functions.
//
//===------------------------------------------------------------------------------------------===//
int main() {
  try {
    // Write some data ...
    write();

    // ... and read it.
    read();

  } catch(std::exception& e) {
    std::cerr << "Errror: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
