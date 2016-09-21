//===-- serialbox/Core/Archive/Archive.h --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Abstract interface for Archives.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_ARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_ARCHIVE_H

#include "serialbox/Core/Exception.h"
#include "serialbox/Core/FieldID.h"
#include "serialbox/Core/StorageView.h"
#include <iosfwd>

namespace serialbox {

///
class Archive {
public:
  ///
  static constexpr const char* ArchiveMetaDataFile = "ArchiveMetaData.json";

  /// \brief Vritual destructor
  virtual ~Archive() {}

  ///
  virtual void write(StorageView& storageView, const FieldID& fieldID) throw(Exception) = 0;

  ///
  virtual void read(StorageView& storageView, const FieldID& fieldID) throw(Exception) = 0;
  
  /// 
  virtual FieldID getNextFieldID(const std::string& field) const noexcept = 0; 

  ///
  virtual void updateMetaData() = 0;

  ///
  virtual const std::string& directory() const = 0;

  ///
  virtual const std::string& name() const = 0;

  ///
  virtual std::ostream& toStream(std::ostream& stream) const = 0;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const Archive& archive) {
    return archive.toStream(stream);
  }
};

} // namespace serialbox

#endif
