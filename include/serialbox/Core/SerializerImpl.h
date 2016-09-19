//===-- serialbox/Core/SerializerImpl.h ---------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the shared implementation of all Serializers.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_SERIALIZER_H
#define SERIALBOX_CORE_SERIALIZER_H

#include "serialbox/Core/Archive/Archive.h"
#include "serialbox/Core/FieldMap.h"
#include "serialbox/Core/MetaInfoMap.h"
#include "serialbox/Core/SavepointImpl.h"
#include <iosfwd>

namespace serialbox {

/// \brief Shared implementation of the Serializers
///
/// Direct usage of this class is discouraged, use the Serializer classes provided by the Frontends
/// instead.
class SerializerImpl {
public:
  /// \brief
  static constexpr const char* SerializerMetaDataFile = "MetaData.json";

  /// \brief Copy constructor [deleted]
  SerializerImpl(const SerializerImpl&) = delete;

  /// \brief Move constructor
  SerializerImpl(SerializerImpl&&) = default;

  /// \brief Copy assignment [deleted]
  SerializerImpl& operator=(const SerializerImpl&) = delete;

  /// \brief Move assignment
  SerializerImpl& operator=(SerializerImpl&&) = default;

  /// \brief Construct from JSON meta-data
  ///
  /// \param mode       Mode of the Serializer
  /// \param directory  Directory of the Archive and meta-data
  ///
  /// This will read MetaData.json to initialize the savepoint vector, the fieldsTable and
  /// globalMetaInfo. Further, it will construct the Archive by reading the ArchiveMetaData.json.
  ///
  /// \throw Exception  Invalid directory or corrupted meta-data files
  SerializerImpl(OpenModeKind mode, std::string directory);

  /// \brief Construct members externally and \b move them in
  ///
  /// \param mode
  /// \param savepoints
  /// \param fieldMap
  /// \param globalMetaInfo
  /// \param archive
  SerializerImpl(OpenModeKind mode, std::vector<SavepointImpl>& savepoints, FieldMap& fieldMap,
                 MetaInfoMap& globalMetaInfo, std::unique_ptr<Archive>& archive);

  /// \brief Convert to stream
  friend std::ostream& operator<<(std::ostream& stream, const SerializerImpl& s);

  /// \brief Convert meta-data to JSON and serialize to MetaData.json
  ///
  /// This will ensure MetaData.json is up-to-date with the in-memory versions of the savepoint
  /// vector, fieldsTable and globalMetaInfo.
  void writeMetaDataToJson();

private:
  /// \brief Construct serializer from JSON
  ///
  /// This will read MetaData.json to initialize the savepoint vector as well as the fieldsTable and
  /// globalMetaInfo. Further, it will construct the Archive by reading ArchiveMetaData.json.
  void constructFromJsonMetaData();

private:
  OpenModeKind mode_;

  std::vector<SavepointImpl> savepoints_;
  FieldMap fieldMap_;

  MetaInfoMap globalMetaInfo_;

  std::unique_ptr<Archive> archive_;
};

} // namespace serialbox

#endif
