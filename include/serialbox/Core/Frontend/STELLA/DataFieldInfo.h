//===-- serialbox/Core/Frontend/STELLA/DataFieldInfo.h ------------------------------*- C++ -*-===//
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

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_DATAFIELDINFO_H
#define SERIALBOX_CORE_FRONTEND_STELLA_DATAFIELDINFO_H

#include "serialbox/Core/Frontend/STELLA/ForwardDecl.h"
#include "serialbox/Core/Frontend/STELLA/IJKBoundary.h"
#include "serialbox/Core/Frontend/STELLA/IJKSize.h"
#include "serialbox/Core/Frontend/STELLA/MetainfoSet.h"
#include "serialbox/Core/Frontend/STELLA/SerializationException.h"
#include "serialbox/Core/Frontend/STELLA/TypeName.h"

namespace serialbox {

namespace stella {

/// \brief Information of a data-field
class DataFieldInfo {
public:
  /// \brief Default constructor
  DataFieldInfo();

  /// \brief Copy constructor
  DataFieldInfo(const DataFieldInfo& other) { *this = other; }

  /// \brief Assignment operator
  DataFieldInfo& operator=(const DataFieldInfo& other);

  /// \brief Initialize with field.
  ///
  /// This general-purpose initializer reads the information from a given data field.
  ///
  /// \param dataField The field of which the information must be read
  template <typename TDataField>
  void Init(const TDataField& dataField) {
    Init(dataField, dataField.name());
  }

  /// \brief Initializer with field and new name.
  ///
  /// This general-purpose initializer recovers the information from a given data field.
  ///
  /// \param dataField The field of which the information must be read
  /// \param name The name of the field when serialized
  template <typename TDataField>
  void Init(const TDataField& dataField, std::string name) {
    // Get the calculation domain
    IJKSize domain;
    domain.Init(dataField.calculationDomain().iSize(), dataField.calculationDomain().jSize(),
                dataField.calculationDomain().kSize());

    IJKSize size;
    size.Init(dataField.storage().allocatedSize().iSize(),
              dataField.storage().allocatedSize().jSize(),
              dataField.storage().allocatedSize().kSize());

    // Get the size of the halo
    IJKBoundary boundary;
    boundary.Init(dataField.boundary().iMinusOffset(), dataField.boundary().iPlusOffset(),
                  dataField.boundary().jMinusOffset(), dataField.boundary().jPlusOffset(),
                  dataField.boundary().kMinusOffset(), dataField.boundary().kPlusOffset());

    // Workaround for boundary() not returning what it should
    const bool haloInI = !(size.iSize() <= domain.iSize());
    const bool haloInJ = !(size.jSize() <= domain.jSize());
    const bool haloInK = !(size.kSize() <= domain.kSize());
    const int iMinus = haloInI ? -boundary.iMinusOffset() : 0;
    const int iPlus = haloInI ? boundary.iPlusOffset() : 0;
    const int jMinus = haloInJ ? -boundary.jMinusOffset() : 0;
    const int jPlus = haloInJ ? boundary.jPlusOffset() : 0;
    const int kMinus = haloInK ? -boundary.kMinusOffset() : 0;
    const int kPlus = haloInK ? boundary.kPlusOffset() : 0;

    // Delegate the initialization
    Init(name, type_name<typename TDataField::ValueType>(), sizeof(typename TDataField::ValueType),
         dataField.storage().rank(), size.iSize(), size.jSize(), size.kSize(), 1, iMinus, iPlus,
         jMinus, jPlus, kMinus, kPlus, 0, 0);
  }

  /// \brief Initializer with sparse information.
  ///
  /// This initializer collects the information given in the multiple parameters.
  ///
  /// \param name              The name of the field
  /// \param type              The data type of the field (e.g. "double")
  /// \param bytesPerElement   The size in bytes of a single entry
  /// \param rank              The number of dimensions of the field
  /// \param iSize             The size in i-direction (including of halo)
  /// \param jSize             The size in j-direction (including of halo)
  /// \param kSize             The size in k-direction (including of halo)
  /// \param lSize             The size in l-direction (including of halo)
  /// \param iMinusHalo        The dimension of the halo in negative i-direction
  /// \param iPlusHalo         The dimension of the halo in positive i-direction
  /// \param jMinusHalo        The dimension of the halo in negative j-direction
  /// \param jPlusHalo         The dimension of the halo in positive j-direction
  /// \param kMinusHalo        The dimension of the halo in negative k-direction
  /// \param kPlusHalo         The dimension of the halo in positive k-direction
  /// \param lMinusHalo        The dimension of the halo in negative l-direction
  /// \param lPlusHalo         The dimension of the halo in positive l-direction
  void Init(std::string name, std::string type, int bytesPerElement, int rank, int iSize, int jSize,
            int kSize, int lSize, int iMinusHalo, int iPlusHalo, int jMinusHalo, int jPlusHalo,
            int kMinusHalo, int kPlusHalo, int lMinusHalo, int lPlusHalo);

  /// \brief Gives read-only access to the metainformation associated with the field
  const MetainfoSet& metainfo() const { return metainfo_; }

  /// \brief The field name
  const std::string& name() const { return name_; }

  /// \brief Sets the name to a value different than that of the initialization field
  void set_Name(std::string name) {
    if(name.empty()) {
      SerializationException exception;
      exception.Init("Passed empty name to DataFieldInfo");
      throw exception;
    }
    name_ = name;
  }

  /// \brief The field scalar type
  const std::string& type() const { return type_; }

  /// \brief The size of the field data in bytes is returned
  int dataSize() const { return bytesPerElement_ * iSize_ * jSize_ * kSize_ * lSize_; }

  /// \brief The dimension in bytes of a single element
  int bytesPerElement() const { return bytesPerElement_; }

  /// \brief The number of dimensions of the field
  int rank() const { return rank_; }

  /// \brief The size of the field in i-direction, including the halo
  int iSize() const { return iSize_; }

  /// \brief The size of the field in j-direction, including the halo
  int jSize() const { return jSize_; }

  /// \brief The size of the field in k-direction, including the halo
  int kSize() const { return kSize_; }

  /// \brief The size of the field in l-direction, including the halo
  int lSize() const { return lSize_; }

  /// \brief The size of the halo in negative i-direction
  int iMinusHaloSize() const { return iMinusHalo_; }

  /// \brief The size of the halo in positive i-direction
  int iPlusHaloSize() const { return iPlusHalo_; }

  /// \brief The size of the halo in negative i-direction
  int jMinusHaloSize() const { return jMinusHalo_; }

  /// \brief The size of the halo in positive i-direction
  int jPlusHaloSize() const { return jPlusHalo_; }

  /// \brief The size of the halo in negative i-direction
  int kMinusHaloSize() const { return kMinusHalo_; }

  /// \brief The size of the halo in positive i-direction
  int kPlusHaloSize() const { return kPlusHalo_; }

  /// \brief The size of the halo in negative i-direction
  int lMinusHaloSize() const { return lMinusHalo_; }

  /// \brief The size of the halo in positive i-direction
  int lPlusHaloSize() const { return lPlusHalo_; }

  /// \brief The number of bytes occupied by the field
  int fieldLength() const { return bytesPerElement_ * iSize_ * jSize_ * kSize_ * lSize_; }

  /// \brief The calculation domain of the field
  IJKSize calculationDomain() const {
    IJKSize size;
    size.Init(iSize_ - iMinusHalo_ - iPlusHalo_, jSize_ - jMinusHalo_ - jPlusHalo_,
              kSize_ - kMinusHalo_ - kPlusHalo_);
    return size;
  }

  /// \brief The total storage of the field (calculation domain and boundary)
  IJKSize size() const {
    IJKSize size;
    size.Init(iSize_, jSize_, kSize_);
    return size;
  }

  /// \brief The size of the halo of the field
  IJKBoundary boundary() const {
    IJKBoundary boundary;
    boundary.Init(-iMinusHalo_, iPlusHalo_, -jMinusHalo_, jPlusHalo_, -kMinusHalo_, kPlusHalo_);
    return boundary;
  }

  /// \brief Add new metainformation
  template <typename ValueType>
  void AddMetainfo(const std::string& key, const ValueType& value) {
    metainfo_.AddMetainfo(key, value);
  }

  /// \brief Comparator operator
  bool operator==(const DataFieldInfo& other) const;

  /// \brief Comparator operator
  inline bool operator!=(const DataFieldInfo& other) const { return !(*this == other); }

  /// \brief Gives a string representation of the object, useful for debugging
  std::string ToString() const;

private:
  std::string name_;
  std::string type_;
  int bytesPerElement_;

  int rank_;

  int iSize_;
  int jSize_;
  int kSize_;
  int lSize_;

  int iPlusHalo_, iMinusHalo_;
  int jPlusHalo_, jMinusHalo_;
  int kPlusHalo_, kMinusHalo_;
  int lPlusHalo_, lMinusHalo_;

  MetainfoSet metainfo_;
};

} // namespace stella

} // namespace serialbox

#endif
