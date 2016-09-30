//===-- Unittest/Cpp/UnittestException.cpp ------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the unittests for the Exception class.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Archive/ArchiveFactory.h"
#include "serialbox/Core/STLExtras.h"
#include <gtest/gtest.h>

namespace serialbox {

// Dummy archive to register
class DummyArchive : public Archive {
public:
  static const std::string Name;

  DummyArchive(OpenModeKind mode, const std::string& directory, const std::string& prefix)
      : mode_(mode), directory_(directory), prefix_(prefix) {}

  virtual FieldID write(StorageView& storageView,
                        const std::string& fieldID) throw(Exception) override {
    return FieldID{"dummy", 0};
  }
  virtual void read(StorageView& storageView, const FieldID& fieldID) throw(Exception) override{};
  virtual void updateMetaData() override {};
  virtual OpenModeKind mode() const override { return mode_; }
  virtual const std::string& directory() const override { return directory_; }
  virtual const std::string& prefix() const override { return prefix_; }
  virtual const std::string& name() const override { return DummyArchive::Name; }
  virtual const std::string& metaDataFile() const override { return directory_; }
  virtual void clear() override {};
  virtual std::ostream& toStream(std::ostream& stream) const override { return stream; }

  static std::unique_ptr<Archive> create(OpenModeKind mode, const std::string& directory,
                                         const std::string& prefix) {
    return std::make_unique<DummyArchive>(mode, directory, prefix);
  }

private:
  OpenModeKind mode_;
  std::string directory_;
  std::string prefix_;
};

const std::string DummyArchive::Name = "DummyArchive";

SERIALBOX_REGISTER_ARCHIVE(DummyArchive, DummyArchive::create)
}

using namespace serialbox;

TEST(ArchiveFactory, Register) {
#ifdef SERIALBOX_RUN_DEATH_TESTS
  ASSERT_DEATH_IF_SUPPORTED(
      ArchiveFactory::getInstance().registerArchive("DummyArchive", DummyArchive::create), "");
#endif
}

TEST(ArchiveFactory, Create) {
  auto mode = OpenModeKind::Write;
  auto archive = ArchiveFactory::getInstance().create("DummyArchive", mode, "directory", "prefix");

  ASSERT_EQ(archive->name(), "DummyArchive");
  ASSERT_EQ(archive->mode(), mode);
  ASSERT_EQ(archive->directory(), "directory");
  ASSERT_EQ(archive->prefix(), "prefix");

  // Archive does not exists
  ASSERT_THROW(ArchiveFactory::getInstance().create("XXX", OpenModeKind::Write, "d", "p"),
               Exception);
}
