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
#include <iosfwd>
#include <memory>
#include <string>


namespace serialbox {

/// \brief Implementation of the Savepoint
///
class SavepointImpl {
public:
  
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointImpl& s);

private:
  std::shared_ptr<FieldID> fieldID_;
};

} // namespace serialbox

#endif
