//===-- serialbox/Core/Frontend/STELLA/DataFieldInfo.cpp ----------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the DataFieldInfo implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Frontend/STELLA/DataFieldInfo.h"

namespace serialbox {

namespace stella {

DataFieldInfo::DataFieldInfo() {
  name_ = "";
  type_ = "";
  bytesPerElement_ = 0;
  rank_ = 0;
  iSize_ = 1;
  jSize_ = 1;
  kSize_ = 1;
  lSize_ = 1;
  iPlusHalo_ = 0;
  jPlusHalo_ = 0;
  kPlusHalo_ = 0;
  lPlusHalo_ = 0;
  iMinusHalo_ = 0;
  jMinusHalo_ = 0;
  kMinusHalo_ = 0;
  lMinusHalo_ = 0;
}

DataFieldInfo& DataFieldInfo::operator=(const DataFieldInfo& other) {
  name_ = other.name_;
  type_ = other.type_;
  bytesPerElement_ = other.bytesPerElement_;
  rank_ = other.rank_;
  iSize_ = other.iSize_;
  jSize_ = other.jSize_;
  kSize_ = other.kSize_;
  lSize_ = other.lSize_;
  iPlusHalo_ = other.iPlusHalo_;
  jPlusHalo_ = other.jPlusHalo_;
  kPlusHalo_ = other.kPlusHalo_;
  lPlusHalo_ = other.lPlusHalo_;
  iMinusHalo_ = other.iMinusHalo_;
  jMinusHalo_ = other.jMinusHalo_;
  kMinusHalo_ = other.kMinusHalo_;
  lMinusHalo_ = other.lMinusHalo_;
  metainfo_ = other.metainfo_;

  return *this;
}

void DataFieldInfo::Init(std::string name, std::string type, int bytesPerElement, int rank,
                         int iSize, int jSize, int kSize, int lSize, int iMinusHalo, int iPlusHalo,
                         int jMinusHalo, int jPlusHalo, int kMinusHalo, int kPlusHalo,
                         int lMinusHalo, int lPlusHalo) {
  name_ = name;
  type_ = type;
  bytesPerElement_ = bytesPerElement;
  rank_ = rank;

  iSize_ = iSize;
  jSize_ = jSize;
  kSize_ = kSize;
  lSize_ = lSize;

  iMinusHalo_ = iMinusHalo;
  iPlusHalo_ = iPlusHalo;
  jMinusHalo_ = jMinusHalo;
  jPlusHalo_ = jPlusHalo;
  kMinusHalo_ = kMinusHalo;
  kPlusHalo_ = kPlusHalo;
  lMinusHalo_ = lMinusHalo;
  lPlusHalo_ = lPlusHalo;
}

bool DataFieldInfo::operator==(const DataFieldInfo& other) const {
  return (bytesPerElement_ == other.bytesPerElement_ && rank_ == other.rank_ &&

          iSize_ == other.iSize_ && jSize_ == other.jSize_ && kSize_ == other.kSize_ &&
          lSize_ == other.lSize_ &&

          iMinusHalo_ == other.iMinusHalo_ && iPlusHalo_ == other.iPlusHalo_ &&
          jMinusHalo_ == other.jMinusHalo_ && jPlusHalo_ == other.jPlusHalo_ &&
          kMinusHalo_ == other.kMinusHalo_ && kPlusHalo_ == other.kPlusHalo_ &&
          lMinusHalo_ == other.lMinusHalo_ && lPlusHalo_ == other.lPlusHalo_ &&

          name_ == other.name_ && type_ == other.type_ && metainfo_ == other.metainfo_);
}

std::string DataFieldInfo::ToString() const {
  std::ostringstream ss;
  ss << name_ << " (" << iSize_ << "x" << jSize_ << "x" << kSize_ << "x" << lSize_ << ") "
     << metainfo_.ToString();
  return ss.str();
}

} // namespace stella

} // namespace serialbox
