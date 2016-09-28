//===-- serialbox/Core/Frontend/STELLA/Serializer.h ---------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Serializer implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H
#define SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H

#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"
#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include "serialbox/Core/Frontend/STELLA/Savepoint.h"
#include <string>
#include <vector>

namespace serialbox {

namespace stella {

/// \enum SerializerOpenMode
/// \brief OpenPolicy of the Serializer
enum SerializerOpenMode {
  SerializerOpenModeRead,
  SerializerOpenModeWrite,
  SerializerOpenModeAppend
};

/// \brief Implementation of the STELLA Serializer
class Serializer {
public:
  Serializer();

  /// \brief Enable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable STELLA_SERIALIZATION_DISABLE to a positive value or by calling the funcion
  /// DisableSerialization. With this function you enable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can be only globally enabled or disabled. There is not way to enable or
  /// disable only a specific serializer.
  static void EnableSerialization() { enabled_ = 1; }

  /// \brief Disable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable STELLA_SERIALIZATION_DISABLE to a positive value or by calling the funcion
  /// DisableSerialization.
  ///
  /// The serialization can be only globally enabled or disabled. There is not way to enable or
  /// disable only a specific serializer.
  static void DisableSerialization() { enabled_ = -1; }

  /// \brief Open mode
  ///
  /// \return The mode is returned as value
  SerializerOpenMode mode() const;

  /// \brief Directory where the data are stored
  ///
  /// \return The directory is returned as copy
  std::string directory() const;

  /// \brief Prefix of the files
  ///
  /// \return The prefix is returned as copy
  std::string prefix() const;

  /// \brief Initializes the serializer
  ///
  /// This method initializes the serializer and prepares it to perform input/output operations.
  ///
  /// The serializer is attached to a specific directory and to a specific prefix, with which all
  /// files read and written will start. There are three modes to open a serializer: read, write and
  /// append. Read will give a read-only access to the serialized data; Write will erase all files
  /// of a previous run of a serializer with same directory and prefix; Append wil import all
  /// existing information and allow the user to add more data.
  ///
  /// \param directory  The directory where the files will be stored (will be created if necessary)
  /// \param prefix     The prefix of the files
  /// \param mode       The read/write/append flag that sets the access mode
  void Init(const std::string& directory, const std::string& prefix, SerializerOpenMode mode);

  /// \brief Read-only access to the metainformation
  MetainfoSet globalMetainfo() const;

private:
  std::vector<Savepoint> savepoints_;
  SerializerImpl* serializerImpl_;

  /// This variable can take three values:
  ///
  ///  0: the variable is not yet initialized -> the serialization is enabled if the environment
  ///     variable STELLA_SERIALIZATION_DISABLE is not set to  a positive value. The first
  ///     serializer which is initialized has to set this value either to +1 or to -1 according to
  ///     the environment.
  /// +1: the serialization is enabled, independently of the environment
  /// -1: the serialization is disabled, independently of the environment
  ///
  /// The value is initialized to 0
  static int enabled_;
};

} // namespace stella

} // namespace serialbox

#endif
