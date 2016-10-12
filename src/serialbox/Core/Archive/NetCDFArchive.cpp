//===-- serialbox/Core/Archive/NetCDFArchive.cpp ------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file implements the archive based on NetCDF.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Compiler.h"
#ifdef SERIALBOX_HAS_NETCDF

#include "serialbox/Core/Archive/NetCDFArchive.h"
#include "serialbox/Core/Logging.h"
#include "serialbox/Core/STLExtras.h"
#include "serialbox/Core/Version.h"
#include "serialbox/Core/Unreachable.h"
#include <boost/algorithm/string.hpp>
#include <netcdf.h>
#include <fstream>
#include <memory>
#include <unordered_map>
#include <vector>

// Check return type of NetCDF function calls
#define NETCDF_CHECK(functionCall)                                                                 \
  if((errorCode = functionCall))                                                                   \
    throw serialbox::Exception("NetCDFArchive: %s", nc_strerror(errorCode));

namespace serialbox {

namespace internal {

static int typeID2NcType(TypeID type) {
  switch(type) {
  case TypeID::Boolean:
    return NC_BYTE;
  case TypeID::Int32:
    return NC_INT;
  case TypeID::Int64:
    return NC_INT64;
  case TypeID::Float32:
    return NC_FLOAT;
  case TypeID::Float64:
    return NC_DOUBLE;
  default:
    throw Exception("cannot convert type '%s' to netCDF type", TypeUtil::toString(type));
  }
}

} // namespace internal

const std::string NetCDFArchive::Name = "NetCDF";

const int NetCDFArchive::Version = 0;

NetCDFArchive::NetCDFArchive(OpenModeKind mode, const std::string& directory,
                             const std::string& prefix)
    : mode_(mode), directory_(directory), prefix_(prefix) {

  LOG(info) << "Creating NetCDFArchive (mode = " << mode_ << ") from directory " << directory_;

  metaDatafile_ = directory_ / ("ArchiveMetaData-" + prefix_ + ".json");

  try {
    bool isDir = boost::filesystem::is_directory(directory_);

    switch(mode_) {
    // We are reading, the directory needs to exist
    case OpenModeKind::Read:
      if(!isDir)
        throw Exception("no such directory: '%s'", directory_.string());
      break;
    // We are writing or appending, create directories if it they don't exist
    case OpenModeKind::Write:
    case OpenModeKind::Append:
      if(!isDir)
        boost::filesystem::create_directories(directory_);
      break;
    }
  } catch(boost::filesystem::filesystem_error& e) {
    throw Exception(e.what());
  }

   readMetaDataFromJson();

  // Remove all files
  if(mode_ == OpenModeKind::Write)
    clear();
}

void NetCDFArchive::updateMetaData() { writeMetaDataToJson(); }

void NetCDFArchive::writeMetaDataToJson() {
  LOG(info) << "Update MetaData of NetCDF Archive";

  json_.clear();

  // Tag versions
  json_["serialbox_version"] =
      100 * SERIALBOX_VERSION_MAJOR + 10 * SERIALBOX_VERSION_MINOR + SERIALBOX_VERSION_PATCH;
  json_["archive_name"] = NetCDFArchive::Name;
  json_["archive_version"] = NetCDFArchive::Version;

  // FieldMap
  for(auto it = fieldMap_.begin(), end = fieldMap_.end(); it != end; ++it)
    json_["field_map"][it->first] = it->second;

  // Write metaData to disk (just overwrite the file, we assume that there is never more than one
  // Archive per data set and thus our in-memory copy is always the up-to-date one)
  std::ofstream fs(metaDatafile_.string(), std::ios::out | std::ios::trunc);

  if(!fs.is_open())
    throw Exception("cannot open file: %s", metaDatafile_);

  fs << json_.dump(2) << std::endl;
  fs.close();
}

void NetCDFArchive::readMetaDataFromJson() {
  LOG(info) << "Reading MetaData for NetCDF archive ... ";

  // Check if metaData file exists
  if(!boost::filesystem::exists(metaDatafile_)) {
    if(mode_ != OpenModeKind::Read)
      return;
    throw Exception("archive meta data not found in directory '%s'", directory_.string());
  }

  std::ifstream fs(metaDatafile_.string(), std::ios::in);
  fs >> json_;
  fs.close();

  int serialboxVersion = json_["serialbox_version"];
  std::string archiveName = json_["archive_name"];
  int archiveVersion = json_["archive_version"];

  // Check consistency
  if(!Version::match(serialboxVersion))
    throw Exception("serialbox version of NetCDF archive (%s) does not match the version "
                    "of the library (%s)",
                    Version::toString(serialboxVersion), SERIALBOX_VERSION_STRING);

  if(archiveName != NetCDFArchive::Name)
    throw Exception("archive is not a NetCDF archive");

  if(archiveVersion != NetCDFArchive::Version)
    throw Exception("NetCDF archive version (%s) does not match the version of the library (%s)",
                    archiveVersion, NetCDFArchive::Version);

  // Deserialize FieldMap
  if(json_.count("field_map")) {
    fieldMap_.clear();
    for(auto it = json_["field_map"].begin(); it != json_["field_map"].end(); ++it)
      fieldMap_.insert({it.key(), static_cast<int>(it.value())});
  }
}

//===------------------------------------------------------------------------------------------===//
//     Writing
//===------------------------------------------------------------------------------------------===//

FieldID NetCDFArchive::write(const StorageView& storageView,
                             const std::string& field) throw(Exception) {
  if(mode_ == OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Write' or 'Append'");

  int ncID, varID, errorCode;

  TypeID type = storageView.type();
  const std::vector<int>& dims = storageView.dims();
  const std::vector<int>& strides = storageView.strides();

  std::size_t numDims = dims.size();
  std::size_t numDimsID = numDims + 1;

  auto it = fieldMap_.find(field);

  FieldID fieldID{field, 0};
  boost::filesystem::path filename = directory_ / (prefix_ + "_" + field + ".nc");

  if(it != fieldMap_.end()) {
    it->second++;
    fieldID.id = it->second;

    // Open file for appending
    NETCDF_CHECK(nc_open(filename.c_str(), NC_WRITE, &ncID));

    // Get the variable
    NETCDF_CHECK(nc_inq_varid(ncID, field.c_str(), &varID));

  } else {
    // Open new file 
    NETCDF_CHECK(nc_create(filename.c_str(), NC_NETCDF4, &ncID));

    // Create dimensions
    std::vector<int> dimsID(numDimsID);

    NETCDF_CHECK(nc_def_dim(ncID, "fieldID", NC_UNLIMITED, &dimsID[0]));
    for(int i = 1; i < dimsID.size(); ++i)
      NETCDF_CHECK(
          nc_def_dim(ncID, ("d" + std::to_string(i - 1)).c_str(), dims[i - 1], &dimsID[i]));

    // Define the variable
    NETCDF_CHECK(nc_def_var(ncID, field.c_str(), internal::typeID2NcType(type), numDimsID,
                            dimsID.data(), &varID));

    // End define mode
    NETCDF_CHECK(nc_enddef(ncID));

    fieldMap_.insert({fieldID.name, fieldID.id});
  }

  // Write data to disk
  std::vector<std::size_t> startp(numDimsID, 0), countp(numDimsID);
  std::vector<std::ptrdiff_t> stridep(numDimsID, 1), imapp(numDimsID);

  startp[0] = fieldID.id;
  countp[0] = 1;
  imapp[0] = storageView.size();

  for(int i = 0; i < numDims; ++i) {
    countp[i + 1] = dims[i];
    imapp[i + 1] = strides[i];
  }

  switch(type) {
  case TypeID::Boolean:
    NETCDF_CHECK(nc_put_varm_text(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                  imapp.data(), storageView.originPtr()));
    break;
  case TypeID::Int32:
    NETCDF_CHECK(nc_put_varm_int(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                 imapp.data(), storageView.originPtrAs<int>()));
    break;
  case TypeID::Int64:
    NETCDF_CHECK(nc_put_varm_long(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                  imapp.data(), storageView.originPtrAs<std::int64_t>()));
    break;
  case TypeID::Float32:
    NETCDF_CHECK(nc_put_varm_float(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                   imapp.data(), storageView.originPtrAs<float>()));
    break;
  case TypeID::Float64:
    NETCDF_CHECK(nc_put_varm_double(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                    imapp.data(), storageView.originPtrAs<double>()));
    break;
  default:
    serialbox_unreachable("type not supported");
  }

  // Close file
  NETCDF_CHECK(nc_close(ncID));
  
  // Update meta-data
  updateMetaData();
  
  LOG(info) << "Successfully wrote field \"" << fieldID.name << "\" (id = " << fieldID.id << ") to "
            << filename.filename();
  return fieldID;
}

//===------------------------------------------------------------------------------------------===//
//     Reading
//===------------------------------------------------------------------------------------------===//

void NetCDFArchive::read(StorageView& storageView, const FieldID& fieldID) const throw(Exception) {
  if(mode_ != OpenModeKind::Read)
    throw Exception("Archive is not initialized with OpenModeKind set to 'Read'");

  LOG(info) << "Attempting to read field \"" << fieldID.name << "\" (id = " << fieldID.id
            << ") via NetCDFArchive ... ";
  
  int ncID, varID, errorCode;
  
  TypeID type = storageView.type();
  const std::vector<int>& dims = storageView.dims();
  const std::vector<int>& strides = storageView.strides();

  std::size_t numDims = dims.size();
  std::size_t numDimsID = numDims + 1;

  // Check if field exists  
  auto it = fieldMap_.find(fieldID.name);
  if(it == fieldMap_.end())
    throw Exception("no field '%s' registered in NetCDFArchive", fieldID.name);
    
  // Check if id is valid  
  if(fieldID.id > it->second) 
    throw Exception("invalid id '%i' of field '%s'", fieldID.id, fieldID.name);
  
  boost::filesystem::path filename = directory_ / (prefix_ + "_" + fieldID.name + ".nc");
  
  // Open file for reading
  NETCDF_CHECK(nc_open(filename.c_str(), NC_NOWRITE, &ncID));
  
  // Get the variable
  NETCDF_CHECK(nc_inq_varid(ncID, fieldID.name.c_str(), &varID));
  
  // Read data from disk
  std::vector<std::size_t> startp(numDimsID, 0), countp(numDimsID);
  std::vector<std::ptrdiff_t> stridep(numDimsID, 1), imapp(numDimsID);

  startp[0] = fieldID.id;
  countp[0] = 1;
  imapp[0] = storageView.size();

  for(int i = 0; i < numDims; ++i) {
    countp[i + 1] = dims[i];
    imapp[i + 1] = strides[i];
  }

  switch(type) {
  case TypeID::Boolean:
    NETCDF_CHECK(nc_get_varm_text(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                  imapp.data(), storageView.originPtr()));
    break;
  case TypeID::Int32:
    NETCDF_CHECK(nc_get_varm_int(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                 imapp.data(), storageView.originPtrAs<int>()));
    break;
  case TypeID::Int64:
    NETCDF_CHECK(nc_get_varm_long(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                  imapp.data(), storageView.originPtrAs<std::int64_t>()));
    break;
  case TypeID::Float32:
    NETCDF_CHECK(nc_get_varm_float(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                   imapp.data(), storageView.originPtrAs<float>()));
    break;
  case TypeID::Float64:
    NETCDF_CHECK(nc_get_varm_double(ncID, varID, startp.data(), countp.data(), stridep.data(),
                                    imapp.data(), storageView.originPtrAs<double>()));
    break;
  default:
    serialbox_unreachable("type not supported");
  }
  
  // Close file
  NETCDF_CHECK(nc_close(ncID));
  
  LOG(info) << "Successfully read field \"" << fieldID.name << "\" (id = " << fieldID.id << ")";  
}

void NetCDFArchive::clear() {
  boost::filesystem::directory_iterator end;
  for(boost::filesystem::directory_iterator it(directory_); it != end; ++it) {
    if(boost::filesystem::is_regular_file(it->path()) &&
       boost::algorithm::starts_with(it->path().filename().string(), prefix_ + "_") &&
       boost::filesystem::extension(it->path()) == ".nc") {

      if(!boost::filesystem::remove(it->path()))
        LOG(warning) << "NetCDFArchive: cannot remove file " << it->path();
    }
  }
  fieldMap_.clear();
}

std::ostream& NetCDFArchive::toStream(std::ostream& stream) const {
  stream << "NetCDFArchive = {\n";
  stream << "  directory: " << directory_.string() << "\n";
  stream << "  mode: " << mode_ << "\n";
  stream << "  prefix: " << prefix_ << "\n";
  stream << "  fieldMap = {\n";
  for(auto it = fieldMap_.begin(), end = fieldMap_.end(); it != end; ++it) 
    stream << "    " << it->first << ": " << it->second << "\n";
  stream << "  }\n";
  stream << "}\n";
  return stream;
}

std::unique_ptr<Archive> NetCDFArchive::create(OpenModeKind mode, const std::string& directory,
                                               const std::string& prefix) {
  return std::make_unique<NetCDFArchive>(mode, directory, prefix);
}

} // namespace serialbox

#endif // SERIALBOX_HAS_NETCDF
