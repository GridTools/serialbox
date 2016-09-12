//===-- Core/Archive/BinaryArchive.cpp ----------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the non-portable binary archive.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Archive/BinaryArchive.h"
#include <boost/utility/string_ref.hpp>
#include <iostream>
#include <fstream>

namespace serialbox {

BinaryArchive::BinaryArchive(boost::filesystem::path directory, OpenModeKind mode) : mode_(mode) {
  // Check path exists (if we are reading)
  try {
    if(mode_ != OpenModeKind::Write && !boost::filesystem::is_directory(directory))
      throw Exception("directory '%s' does not exist", directory.string());
  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }
  directory_ = std::move(directory);
 
  // Deserialize meta data
  if(mode_ != OpenModeKind::Write) {
    // TODO...
  }
}

void BinaryArchive::write(StorageView& storageView, const FieldID& fieldID) throw(Exception) {
  if(mode_ != OpenModeKind::Write)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write'");

  // Check if a field with given id and name already exists
  auto it = fieldTable_.find(fieldID.name);

  std::string filename((directory_ / (fieldID.name + ".dat")).string());
  std::ofstream fs;
  std::vector<Byte> binaryData;

  // Field already exists ?
  if(it != fieldTable_.end()) {
    // Do we append at the end?
    if(fieldID.id >= it->second.size()) {
      
      // Update associating meta data
//      it->second.push_back(FileOffsetType{offset, "__checksum__"});      
    }
    // Replace data
    else {
      
      // Update associating meta data
//      it->second[fieldID.id] = FileOffsetType{offset, "__checksum__"};
    }
  }
  // Field does not exist, create new file and append data
  else {
    // Create contiguous memory
    try {
      binaryData.resize(storageView.size());
    } catch(std::bad_alloc&) {
      throw Exception("out of memory");
    }

    Byte* dataPtr = binaryData.data();
    const int bytesPerElement = storageView.bytesPerElement();

    // Copy field into contiguous memory
    for(auto it = storageView.begin(), end = storageView.end(); it != end;
        ++it, dataPtr += bytesPerElement)
      std::memcpy(it.ptr(), dataPtr, bytesPerElement);

    fs.open(filename, std::ios::out | std::ios::binary);

    // Update associating meta data
    fieldTable_.insert(FieldTable::value_type(
        fieldID.name, std::vector<FileOffsetType>(1, FileOffsetType{0, "__checksum__"})));
  }

  if(!fs.is_open())
    throw Exception("cannot open file: '%s'", filename);

  // Write binaryData to disk
  fs.write(binaryData.data(), binaryData.size());
  fs.close();

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Archive per data set)
  
  // TODO...
}

void BinaryArchive::read(StorageView& storageView, const FieldID& fieldID) throw(Exception) {
  if(mode_ == OpenModeKind::Write)
    throw Exception("Archive is initialized with OpenModeKind set to 'Write'");
  
  // Check if field exists
  auto it = fieldTable_.find(fieldID.name);
  if(it == fieldTable_.end())
    throw Exception("no field '%s' registered in BinaryArchive (%s)", fieldID.name,
                    directory_.string());
  
  const std::vector<FileOffsetType>& fileOffset = it->second;
  
  // Check if id is valid
  if(fileOffset.size() >= fieldID.id)
    throw Exception("invalid id '%i' of field '%s'", fieldID.id, fieldID.name);
     
//  auto offset = fileOffset[fieldID.id].offset;
//  const std::string& checksum = fileOffset[fieldID.id].checksum;
  
}

} // namespace serialbox
// Replace
