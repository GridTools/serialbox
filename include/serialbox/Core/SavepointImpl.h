//===-- serialbox/Core/SavepointImpl.h ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation of all Savepoints.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTIMPL_H
#define SERIALBOX_CORE_SAVEPOINTIMPL_H

#include "serialbox/Core/FieldID.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <iosfwd>

namespace serialbox {

/// \brief Shared implementation of the Savepoints
///
/// Direct usage of this class is discouraged, use the Savepoint classes provided by the Frontends
/// instead.
class SavepointImpl {
public:
  /// \brief Copy constructor [deleted]
  SavepointImpl(const SavepointImpl&) = delete;

  /// \brief Move constructor
  SavepointImpl(SavepointImpl&&) = default;

  /// \brief Copy assignment [deleted]
  SavepointImpl& operator=(const SavepointImpl&) = delete;

  /// \brief Move assignment
  SavepointImpl& operator=(SavepointImpl&&) = default;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

private:
  std::string name_;            ///< Name of this savepoint
  MetaInfoMap metaInfo_;        ///< Meta-information of this savepoint
  std::vector<FieldID> fields_; ///< Fields captured by this savepoint
};

} // namespace serialbox

#endif
