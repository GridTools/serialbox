//===-- serialbox/Core/SavepointCollectionImpl.h ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the implementation of the SavepointCollection.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SAVEPOINTCOLLECTIONIMPL_H
#define SERIALBOX_CORE_SAVEPOINTCOLLECTIONIMPL_H

#include <iosfwd>
#include <string>

namespace serialbox {

/// \brief Implementation of the SavepointCollection
class SavepointCollectionImpl {
public:
  
  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SavepointCollectionImpl& s);

private:
  std::string name_;
};

} // namespace serialbox

#endif
