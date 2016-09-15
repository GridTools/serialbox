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
/// This file contains the implementation of the Savepoint.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTIMPL_H
#define SERIALBOX_CORE_SAVEPOINTIMPL_H

#include "serialbox/Core/FieldID.h"
#include "serialbox/Core/MetaInfoMap.h"
#include <iosfwd>

namespace serialbox {

/// \brief Shared implementation of the Savepoint
///
/// Direct usage of this class is discouraged, use the Savepoint classes provided by the Frontends
/// instead.
class SavepointImpl {
public:
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

private:
  MetaInfoMap metaInfo_;
  std::vector<FieldID> fields_;
};

} // namespace serialbox

#endif
