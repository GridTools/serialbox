//===-- Utility/CInterfaceTestBase.h ------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file provides a template to setup C Interface tests and easily check for errors.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_UTILITY_CINTERFACETESTBASE_H
#define SERIALBOX_UTILITY_CINTERFACETESTBASE_H

#include "Utility/FileUtility.h"
#include <gtest/gtest.h>

namespace serialbox {

namespace unittest {

/// \brief Simplify construction of C Interface tests by automatically registering a
/// FatalErrorHandler which allows to query the error messages
///
/// In addition, the a unqiue directory for each test will be created (i.e same behaviour as 
/// SerializerTestBase).
class CInterfaceTestBase : public ::testing::Test {
public:
  std::unique_ptr<Directory> directory;
  
protected:
  /// \brief Install the ErrorHandler
  virtual void SetUp() override;

  /// \brief Release the ErrorHandler
  virtual void TearDown() override;

  /// \brief Check if an error occured  
  bool hasError() const noexcept;   
  
  /// \brief Check if an error occured and reset the error counter
  bool hasErrorAndReset() const noexcept;

  /// \brief Get last error message
  std::string getLastErrorMsg() const;
};

} // namespace unittest

} // namespace serialbox

#endif
