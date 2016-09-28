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
#include <string>
#include <iosfwd>

namespace serialbox {

namespace stella {

/// \brief Implementation of the STELLA Savepoint
class Savepoint {
public:
  /// \brief Construct with SavepointImpl (lifetime of SavepointImpl has to be managed externally)
  Savepoint(SavepointImpl* savepointImpl);

  /// \brief Copy assignment
  Savepoint& operator=(const Savepoint& other);

  /// \brief Access to the name
  std::string name() const;

  /// \brief Access to the metainfo
  ///
  /// The meta-information is constructed from the underlying MetaInfoMap of the SavepointImpl.
  MetainfoSet metainfo() const;

  /// \brief Compare equal
  bool operator==(const Savepoint& other) const;

  /// \brief Compare unequal
  bool operator!=(const Savepoint& other) const;
  
  /// \brief Convert to string
  std::string ToString() const;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& out, const Savepoint& sp);
  
private:
  SavepointImpl* savepointImpl_;
};

} // namespace stella

} // namespace serialbox

#endif
