//===-- serialbox/Core/Frontend/STELLA/Savepoint.h ----------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Savepoint implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_SAVEPOINT_H
#define SERIALBOX_CORE_FRONTEND_STELLA_SAVEPOINT_H

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include <iosfwd>
#include <string>

namespace serialbox {

namespace stella {

/// \brief Implementation of the STELLA Savepoint
class Savepoint {
public:
  ~Savepoint();

  /// \brief Construct empty savepoint with name ´name´
  Savepoint();

  /// \brief Initialize the savepoint
  ///
  /// This method prepares the savepoint for usage and gives a name, which is the only required
  /// information for the savepoint to be usable. Metainformation can be added after the
  /// initialization has been performed.
  ///
  /// A savepoint can be initialized multiple times. In this case, every initialization removes all
  /// metainformation and sets a new name.
  ///
  /// \param name The name of the savepoint
  void Init(const std::string& name);

  /// \brief Construct with SavepointImpl (lifetime of SavepointImpl has to be managed externally)
  explicit Savepoint(SavepointImpl* savepointImpl);

  /// \brief Copy constructor
  Savepoint(const Savepoint& other);

  /// \brief Copy assignment
  Savepoint& operator=(const Savepoint& other);

  /// \brief Add metainformation to the savepoint
  ///
  /// After this call a new key-value pair is registered as metainformation in the savepoint. The
  /// order in which the metainformation is added is irrelevant.
  ///
  /// \param key    The key of the new metainformation
  /// \param value  The value of the new metainformaiton
  ///
  /// \throw SerializationException The key exists already
  /// @{
  void AddMetainfo(const std::string& key, const bool& value);
  void AddMetainfo(const std::string& key, const int& value);
  void AddMetainfo(const std::string& key, const float& value);
  void AddMetainfo(const std::string& key, const double& value);
  void AddMetainfo(const std::string& key, const std::string& value);
  void AddMetainfo(const std::string& key, const char* value) {
    AddMetainfo(key, std::string(value));
  }
  /// @}

  /// \brief Access to the name
  std::string name() const;

  /// \brief Access to the metainfo
  ///
  /// The meta-information is constructed from the underlying MetaInfoMap of the SavepointImpl.
  const MetainfoSet& metainfo() const { return metainfo_; }

  /// \brief Compare equal
  bool operator==(const Savepoint& other) const;

  /// \brief Compare unequal
  bool operator!=(const Savepoint& other) const;

  /// \brief Convert to string
  std::string ToString() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& out, const Savepoint& sp);

  /// \brief Get implementation pointer
  SavepointImpl* getImpl() const { return savepointImpl_; }

private:
  bool owner_;
  SavepointImpl* savepointImpl_;
  MetainfoSet metainfo_;
};

} // namespace stella

} // namespace serialbox

#endif
