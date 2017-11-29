//===-- serialbox/core/frontend/stella/Serializer.h ---------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the Serializer implementation of the STELLA frontend.
///
//===------------------------------------------------------------------------------------------===//

#ifndef SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H
#define SERIALBOX_CORE_FRONTEND_STELLA_SERIALIZER_H

#ifdef SERIALBOX_HAS_STELLA
#include "DataFieldStorageFormat.h"
#include "DataFieldStorageStrides.h"
#endif

#include "serialbox/core/frontend/stella/DataFieldInfo.h"
#include "serialbox/core/frontend/stella/ForwardDecl.h"
#include "serialbox/core/frontend/stella/KBoundary.h"
#include "serialbox/core/frontend/stella/MetainfoSet.h"
#include "serialbox/core/frontend/stella/Savepoint.h"
#include "serialbox/core/frontend/stella/SerializationException.h"
#include <boost/shared_ptr.hpp>
#include <map>
#include <sstream>
#include <string>
#include <vector>

namespace serialbox {

namespace stella {

/// \enum SerializerOpenMode
/// \brief OpenPolicy of the Serializer
///
/// \ingroup STELLA
enum SerializerOpenMode {
  SerializerOpenModeRead,
  SerializerOpenModeWrite,
  SerializerOpenModeAppend
};

/// \brief Implementation of the STELLA Serializer
///
/// \ingroup STELLA
class Serializer {
public:
  Serializer();

  /// \brief Enable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `STELLA_SERIALIZATION_DISABLE` to a positive value or by calling the function
  /// DisableSerialization. With this function you enable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can be only globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void EnableSerialization();

  /// \brief Disable serialization
  ///
  /// Serialization is enabled by default, but it can be disabled either by setting the environment
  /// variable `STELLA_SERIALIZATION_DISABLE` to a positive value or by calling the funtcion
  /// DisableSerialization. With this function you disable the serialization independently of the
  /// current environment.
  ///
  /// The serialization can be only globally enabled or disabled. There is no way to enable or
  /// disable only a specific serializer.
  static void DisableSerialization();

  /// \brief Open mode
  ///
  /// \return The mode is returned as value
  SerializerOpenMode mode() const;

  /// \brief Directory where the data are stored
  ///
  /// \return The directory is returned as copy
  std::string directory() const;

  /// \brief Prefix of the files
  ///
  /// \return The prefix is returned as copy
  std::string prefix() const;

  /// \brief Initializes the serializer
  ///
  /// This method initializes the serializer and prepares it to perform input/output operations.
  ///
  /// The serializer is attached to a specific directory and to a specific prefix, with which all
  /// files read and written will start. There are three modes to open a serializer: read, write and
  /// append. Read will give a read-only access to the serialized data; Write will erase all files
  /// of a previous run of a serializer with same directory and prefix; Append wil import all
  /// existing information and allow the user to add more data.
  ///
  /// \param directory  The directory where the files will be stored (will be created if necessary)
  /// \param prefix     The prefix of the files
  /// \param mode       The read/write/append flag that sets the access mode
  void Init(const std::string& directory, const std::string& prefix, SerializerOpenMode mode);

  //===----------------------------------------------------------------------------------------===//
  //     Global meta-information
  //===----------------------------------------------------------------------------------------===//

  /// \brief Read-only access to the metainformation
  const MetainfoSet& globalMetainfo() const { return globalMetainfo_; }

  /// Adds a key-value metainformation pair to the serializer.
  ///
  /// @throw SerializationException The type of the value is not supported.
  template <typename ValueType>
  void AddMetainfo(const std::string& key, ValueType value) {
    globalMetainfo_.AddMetainfo(key, value);
  }

  //===----------------------------------------------------------------------------------------===//
  //     Register fields
  //===----------------------------------------------------------------------------------------===//

  /// \brief Register a field into the serializer
  ///
  /// It is possible to register a field multiple times, as long as all properties of the field
  /// always match.
  ///
  /// \param name            The name of the field
  /// \param type            The name of the data type (e.g. "double", "float")
  /// \param bytesPerElement The size in bytes of a scalar value (e.g. 8 for doubles)
  /// \param iSize           The size of the first dimension
  /// \param jSize           The size of the second dimension
  /// \param kSize           The size of the third dimension
  /// \param lsize           The size of the fourth dimension
  /// \param iMinusHalo      The dimension of the halo in negative i-direction
  /// \param iPlusHalo       The dimension of the halo in positive i-direction
  /// \param jMinusHalo      The dimension of the halo in negative j-direction
  /// \param jPlusHalo       The dimension of the halo in positive j-direction
  /// \param kMinusHalo      The dimension of the halo in negative k-direction
  /// \param kPlusHalo       The dimension of the halo in positive k-direction
  /// \param lMinusHalo      The dimension of the halo in negative l-direction
  /// \param lPlusHalo       The dimension of the halo in positive l-direction
  ///
  /// \return The method returns true if the field is new, false if the field was already
  ///         registered with matching information.
  ///
  /// \throw SerializationException   A field with the same name but different properties is
  ///                                 registered already
  bool RegisterField(const std::string& name, std::string type, int bytesPerElement, int iSize,
                     int jSize, int kSize, int lSize, int iMinusHalo, int iPlusHalo, int jMinusHalo,
                     int jPlusHalo, int kMinusHalo, int kPlusHalo, int lMinusHalo, int lPlusHalo);

  /// \brief Gives access to field info
  ///
  /// \throw SerializationException   The field is not registered
  const DataFieldInfo& FindField(const std::string& fieldname) const;

  /// \brief Adds a key-value metainformation pair to a field
  ///
  /// \throw SerializationException   The field is not found in table or the type is not supported
  /// @{
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, bool value);
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, int value);
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, float value);
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, double value);
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, std::string value);
  void AddFieldMetainfo(const std::string& fieldname, const std::string& key, const char* value) {
    AddFieldMetainfo(fieldname, key, std::string(value));
  }
  /// @}

  /// \brief Gives access to the registered fields
  ///
  /// This function fills and returns a vector with the names of the registered fields
  ///
  /// \return The vector with the names of the fields is returned
  std::vector<std::string> fieldnames() const;

  /// \brief Gives access to the list of savepoints
  ///
  /// \return A constant reference to the underlying savepoints vector is returned
  const std::vector<Savepoint>& savepoints() const { return savepoints_; }

  /// \brief Gives all fields which are available at the given savepoint and return their names
  ///
  /// If the savepoint does not exist in the serializer returns
  /// an empty vector and does not throw any exceptions.
  ///
  /// \return A vector with the names of the available fields is returned
  std::vector<std::string> FieldsAtSavepoint(const Savepoint& savepoint) const;

  //===----------------------------------------------------------------------------------------===//
  //     Writing
  //===----------------------------------------------------------------------------------------===//

  /// \brief Serializes data to file
  ///
  /// This method serializes the content of the given array as the field  with the given name at the
  /// provided savepoint.
  ///
  /// \param fieldName  The name of the field
  /// \param savepoint  The savepoint at which the field will be saved
  /// \param pData      The pointer to the beginning of the data
  /// \param iStride    The offset in bytes from one point to the next in i direction
  /// \param jStride    The offset in bytes from one point to the next in j direction
  /// \param kStride    The offset in bytes from one point to the next in k direction
  /// \param lStride    The offset in bytes from one point to the next in l direction
  ///
  /// \throw SerializationException   The field is not registered or it is already present at the
  ///                                 given savepoint
  void WriteField(const std::string& fieldName, const Savepoint& savepoint, const void* pData,
                  int iStride, int jStride, int kStride, int lStride);

#ifdef SERIALBOX_HAS_STELLA
  /// \brief Serializes a data field
  ///
  /// This overload of the method works only with STELLA data types. This will automatically
  /// register the field with the provided name and serialize the field. If the provided name is
  /// empty the name of the data field will be used.
  ///
  /// CUDA fields will be synchronized to the host before being accessed. They are \b not
  /// synchronized back to the device at the end.
  ///
  /// See the main WriteField method to see other cases of exception raising.
  ///
  /// \param name       The name of the field (if empty the name of the field object is used)
  /// \param field      The STELLA data type contianing the information
  /// \param savepoint  The savepoint at which the field will be registed.
  ///
  /// \throw SerializationException The registration fails because of inconsistent registration of
  ///                               fields with same name and different properties or the field is
  ///                               already saved at the savepoint
  template <typename TDataField>
  void WriteField(std::string name, const TDataField& field, const Savepoint& savepoint);
#endif

  //===----------------------------------------------------------------------------------------===//
  //     Reading
  //===----------------------------------------------------------------------------------------===//

  /// \brief Deserializes data from file
  ///
  /// This method reads the contents of the file corresponging to the given field name to the given
  /// savepoint and places the data into the provided array, which must already be allocated.
  ///
  ///
  /// \param fieldName      The name of the field
  /// \param savepoint      The savepoint at which the field will be read
  /// \param pData T        The pointer to the beginning of the data
  /// \param iStride        The offset in bytes from one point to the next in i direction
  /// \param jStride        The offset in bytes from one point to the next in j direction
  /// \param kStride        The offset in bytes from one point to the next in k direction
  /// \param lStride        The offset in bytes from one point to the next in l direction
  /// \param alsoPrevious   Whether to search in previous savepoints with the same name until a
  ///                       valid entry is found [IGNORED]
  ///
  /// \throw SerializationException The field is not registered in the serializer or it is not saved
  ///                               at the savepoint
  void ReadField(const std::string& fieldName, const Savepoint& savepoint, void* pData, int iStride,
                 int jStride, int kStride, int lStride, bool alsoPrevious = false) const;

#ifdef SERIALBOX_HAS_STELLA
  /// \brief Deserializes a data field
  ///
  /// This overload of the method works only with STELLA data types. If the provided name is empty
  /// the name of the data field will be used. The field must be initialized already with the
  /// correct size and boundaries. If these do not match with the serialized field a
  /// SerializationException is raised.
  ///
  /// CUDA fields will be synchronized to the host before being accessed. They are \b not
  /// synchronized back to the device at the end.
  ///
  /// See the main ReadField method to see cases of exception raising.
  ///
  /// \param name           The name of the field (if empty the name of the field object is used)
  /// \param field          The STELLA data type where the data will be loaded
  /// \param savepoint      The savepoint at which the field will be registed.
  /// \param alsoPrevious   Whether to search in previous savepoints with the same name until a
  ///                       valid entry is found [IGNORED]
  template <typename TDataField>
  void ReadField(std::string name, TDataField& field, const Savepoint& savepoint,
                 bool alsoPrevious = false) const;
#endif

  /// \brief Convert to string
  std::string ToString() const;

  /// \brief Get implementation pointer
  boost::shared_ptr<SerializerImpl>& getImpl() { return serializerImpl_; }
  const boost::shared_ptr<SerializerImpl>& getImpl() const { return serializerImpl_; }

private:
  // Implementation pointer
  boost::shared_ptr<SerializerImpl> serializerImpl_;

  // These data-strucures allow to return refrences but do not actually own any data but they need
  // to be kept in sync with the data from serializerImpl!
  MetainfoSet globalMetainfo_;
  std::vector<Savepoint> savepoints_;
  std::map<std::string, DataFieldInfo> datafieldInfos_;

  // This variable can take three values:
  //
  //  0: the variable is not yet initialized -> the serialization is enabled if the environment
  //     variable STELLA_SERIALIZATION_DISABLE is not set to  a positive value. The first
  //     serializer which is initialized has to set this value either to +1 or to -1 according to
  //     the environment.
  // +1: the serialization is enabled, independently of the environment
  // -1: the serialization is disabled, independently of the environment
  //
  // The value is initialized to 0
  static int enabled_;
};

//===------------------------------------------------------------------------------------------===//
//     Implementation of template methods
//===------------------------------------------------------------------------------------------===//

#ifdef SERIALBOX_HAS_STELLA
template <typename TDataField>
void Serializer::WriteField(std::string name, const TDataField& field, const Savepoint& savepoint) {

  if(name.empty())
    name = field.name();

  // Retrieve size, boundaries and strides
  IJKSize size;
  size.Init(field.storage().allocatedSize().iSize(), field.storage().allocatedSize().jSize(),
            field.storage().allocatedSize().kSize());

  IJKBoundary boundary;
  boundary.Init(field.boundary().iMinusOffset(), field.boundary().iPlusOffset(),
                field.boundary().jMinusOffset(), field.boundary().jPlusOffset(),
                field.boundary().kMinusOffset(), field.boundary().kPlusOffset());

  const int bytesPerElement = sizeof(typename TDataField::ValueType);

  DataFieldStorageStrides<typename TDataField::StorageFormat::StorageOrder> strides;
  strides.Init(field.storage().paddedSize());
  const int iStride = strides.ComputeStride(1, 0, 0) * bytesPerElement;
  const int jStride = strides.ComputeStride(0, 1, 0) * bytesPerElement;
  const int kStride = strides.ComputeStride(0, 0, 1) * bytesPerElement;

  // Register field
  this->RegisterField(name, type_name<typename TDataField::ValueType>(), bytesPerElement,
                      size.iSize(), size.jSize(), size.kSize(), 1, -boundary.iMinusOffset(),
                      boundary.iPlusOffset(), -boundary.jMinusOffset(), boundary.jPlusOffset(),
                      -boundary.kMinusOffset(), boundary.kPlusOffset(), 0, 0);

  this->WriteField(name, savepoint, field.storage().pStorageBase(), iStride, jStride, kStride, 0);
}

template <typename TDataField>
void Serializer::ReadField(std::string name, TDataField& field, const Savepoint& savepoint,
                           bool alsoPrevious) const {
  typedef typename TDataField::ValueType ValueType;

  if(name.empty())
    name = field.name();

  // Get info of serialized field
  const DataFieldInfo& info = FindField(name);
  const int bytesPerElement = sizeof(typename TDataField::ValueType);

  // Get strides according to STELLA
  DataFieldStorageStrides<typename TDataField::StorageFormat::StorageOrder> stridesSTELLA;
  stridesSTELLA.Init(field.storage().paddedSize());

  // Get rid of degenerated dimensions and fix strides
  std::vector<int> allSizes, size, allStrides, strides;
  allSizes.push_back(field.storage().allocatedSize().iSize());
  allSizes.push_back(field.storage().allocatedSize().jSize());
  allSizes.push_back(field.storage().allocatedSize().kSize());
  allSizes.push_back(0);
  allStrides.push_back(stridesSTELLA.ComputeStride(1, 0, 0) * bytesPerElement);
  allStrides.push_back(stridesSTELLA.ComputeStride(0, 1, 0) * bytesPerElement);
  allStrides.push_back(stridesSTELLA.ComputeStride(0, 0, 1) * bytesPerElement);
  for (int i = 0; i < 4; ++i) {
      if (allSizes[i] > 1)
      {
          size.push_back(allSizes[i]);
          strides.push_back(allStrides[i]);
      }
  }
  size.resize(4, 0);

  IJKBoundary boundary;
  boundary.Init(field.boundary().iMinusOffset(), field.boundary().iPlusOffset(),
                field.boundary().jMinusOffset(), field.boundary().jPlusOffset(),
                field.boundary().kMinusOffset(), field.boundary().kPlusOffset());


  if((info.iSize() != size[0]) || (info.jSize() != size[1]) ||
     (info.kSize() != size[2]) || (info.lSize() != 0)) {
    // Throw exception
    std::ostringstream errorstr;
    errorstr << "Error: the requested field " << name << " has a different size than"
             << " the provided data field.\n";
    errorstr << "Registerd as: " << info.iSize() << "x" << info.jSize() << "x" << info.kSize()
             << "x" << info.lSize() << "\n";
    errorstr << "Given       : " << size[0] << "x" << size[1] << "x" << size[2]
             << "x" << size[3] << "x0" << "\n";
    SerializationException exception;
    exception.Init(errorstr.str());
    throw exception;
  }

  // Check data type
  if(info.type() != type_name<ValueType>()) {
    // Throw exception
    std::ostringstream errorstr;
    errorstr << "Error: the requested field " << name << " has different type than"
             << " the provided data field (expected " << info.type() << ", got "
             << type_name<ValueType>() << ")";
    SerializationException exception;
    exception.Init(errorstr.str());
    throw exception;
  }

  // This hack is used to retrieve the storage pointer for the host memory in case of GPU fields
  // This will synchronize the host storage
  ValueType* data =
      &field(boundary.iMinusOffset(), boundary.jMinusOffset(), boundary.kMinusOffset());

  // Perform the read
  this->ReadField(name, savepoint, data, strides[0], strides[1], strides[2], 0, alsoPrevious);
}
#endif

} // namespace stella

} // namespace serialbox

#endif
