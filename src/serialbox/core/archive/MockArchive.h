//===-- serialbox/core/archive/MockArchive.h ----------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the Mock Archive.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_ARCHIVE_MOCKARCHIVE_H
#define SERIALBOX_CORE_ARCHIVE_MOCKARCHIVE_H

#include "serialbox/core/archive/Archive.h"
#include <random>

namespace serialbox {

/// \brief Mock archive
///
/// This archive does not actually write or read any data from disk, instead it fills the
/// fields with random data. A simple multiplicative congruential pseudo-random number generator
/// (`std::minstd_rand`) is used and seeded with current time. The returned random variables
/// range from -1.0 to 1.0 (floating point) or 0 to 100 for integers.
///
/// This archive is only supported in OpenModeKind::Read.
///
/// \ingroup core
class MockArchive : public Archive {
public:
  /// \brief Name of the Mock archive
  static const std::string Name;

  /// \brief Initialize the archive
  MockArchive(OpenModeKind mode);

  /// \name Archive implementation
  /// \see Archive
  /// @{
  virtual FieldID write(const StorageView& storageView, const std::string& fieldID,
                        const std::shared_ptr<FieldMetainfoImpl> info) throw(Exception) override;

  virtual void read(StorageView& storageView, const FieldID& fieldID,
                    std::shared_ptr<FieldMetainfoImpl> info) const throw(Exception) override;

  virtual void updateMetaData() override {}

  virtual OpenModeKind mode() const override { return mode_; }

  virtual const std::string& directory() const override { return directory_; }

  virtual const std::string& prefix() const override { return prefix_; }

  virtual const std::string& name() const override { return MockArchive::Name; }

  virtual const std::string& metaDataFile() const override { return metaDataFile_; }

  virtual std::ostream& toStream(std::ostream& stream) const override;

  virtual void clear() override {}

  virtual bool isReadingThreadSafe() const override { return true; }

  virtual bool isWritingThreadSafe() const override { return true; }

  virtual bool isSlicedReadingSupported() const override { return false; }
  /// @}

private:
  OpenModeKind mode_;
  std::string directory_;
  std::string prefix_;
  std::string metaDataFile_;
};

} // namespace serialbox

#endif
