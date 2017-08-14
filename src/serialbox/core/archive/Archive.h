//===-- serialbox/core/archive/Archive.h --------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the abstract interface for Archives.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_ARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_ARCHIVE_H

#include "serialbox/core/Exception.h"
#include "serialbox/core/FieldID.h"
#include "serialbox/core/FieldMetainfoImpl.h"
#include "serialbox/core/StorageView.h"
#include "serialbox/core/Type.h"
#include <iosfwd>

namespace serialbox {

/// \brief Abstract interface for Archives
///
/// \ingroup core
class Archive {
public:
  /// \brief Vritual destructor
  virtual ~Archive() {}

  /// \brief Write the `field` given by `storageView` to disk
  ///
  /// The returned `FieldID` has to uniquely identify the field i.e it should be possible to read
  /// the exact same piece of data when later passing the `FieldID` to the read method.
  ///
  /// \param storageView    Abstract StorageView of the underlying data
  /// \param field          Name of the field
  /// \param info           Field meta-information (can be a `nullptr`)
  /// \return Unique identidier of the field
  virtual FieldID write(const StorageView& storageView, const std::string& field,
                        const std::shared_ptr<FieldMetainfoImpl> info) = 0;

  /// \brief Read the field identified by `fieldID` and given by `storageView` from disk
  ///
  /// \param storageView    Abstract StorageView of the underlying data
  /// \param fieldID        Name and and Id of the field
  /// \param info           Field meta-information (can be a `nullptr`)
  virtual void read(StorageView& storageView, const FieldID& fieldID,
                    std::shared_ptr<FieldMetainfoImpl> info) const = 0;

  /// \brief Update the meta-data on disk
  virtual void updateMetaData() = 0;

  /// \brief Name of the archive
  virtual const std::string& name() const = 0;

  /// \brief Open-policy of the archive
  virtual OpenModeKind mode() const = 0;

  /// \brief Directory to write/read files
  virtual const std::string& directory() const = 0;

  /// \brief Prefix of all files
  virtual const std::string& prefix() const = 0;

  /// \brief Full file path to the meta-data file
  virtual const std::string& metaDataFile() const = 0;

  /// \brief Clear the archive i.e remove all data from disk and reset the internal data-structures
  virtual void clear() = 0;

  /// \brief Indicate whether it's safe for multiple threads to call Archive::read
  virtual bool isReadingThreadSafe() const { return false; }

  /// \brief Indicate whether it's safe for multiple threads to call Archive::write
  virtual bool isWritingThreadSafe() const { return false; }

  /// \brief Indicate whether the archive supports `StorageViews` with attached \ref Slice "slices"
  virtual bool isSlicedReadingSupported() const { return false; }

  /// \brief Convert the archive to stream
  virtual std::ostream& toStream(std::ostream& stream) const = 0;

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const Archive& archive) {
    return archive.toStream(stream);
  }
};

} // namespace serialbox

#endif
