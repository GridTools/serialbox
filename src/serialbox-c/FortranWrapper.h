/*===-- serialbox-c/FortranWrapper.h ------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains some abstractions for the Fortran interface.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_FORTRANWRAPPER_H
#define SERIALBOX_C_FORTRANWRAPPER_H

/**
 * \ingroup serialboxC
 * @{
 *
 * \defgroup fortran_wrapper Fortran wrapper methods
 * @{
 */

#ifdef __cplusplus
extern "C" {
#endif

/*===------------------------------------------------------------------------------------------===*\
 *     Serializer
\*===------------------------------------------------------------------------------------------===*/

void serialboxFortranSerializerWrite(void* serializer, const void* savepoint,
                                    const char* name, void* originPtr,
                                    int istride, int jstride, int kstride, int lstride);

void serialboxFortranSerializerRead(void* serializer, const void* savepoint,
                                    const char* name, void* originPtr,
                                    int istride, int jstride, int kstride, int lstrides);
/**
 * \brief Add a global meta-information `key=value` pair to the Serializer
 *
 * This function corresponds to `fs_add_serializer_metainfo_X`
 *
 * \param serializer  Serializer to use
 * \param key         Key of the new element
 * \param value       Object to be copied to the value of the new element
 * @{
 */
void serialboxFortranSerializerAddMetaInfoBoolean(void* serializer, const char* key, int value);
void serialboxFortranSerializerAddMetaInfoInt32(void* serializer, const char* key, int value);
void serialboxFortranSerializerAddMetaInfoFloat32(void* serializer, const char* key, float value);
void serialboxFortranSerializerAddMetaInfoFloat64(void* serializer, const char* key, double value);
void serialboxFortranSerializerAddMetaInfoString(void* serializer, const char* key,
                                                 const char* value);
/** @} */

/**
 * \brief Register `field` within the serializer
 *
 * This function corresponds to `fs_register_field`
 *
 * \param serializer        Serializer to use
 * \param name              The name of the field
 * \param type              TypeID of the field (\ref serialboxTypeID)
 * \param bytesPerElement   The size in bytes of a scalar value (e.g. 8 for doubles)
 * \param iSize             The size of the first dimension
 * \param jSize             The size of the second dimension
 * \param kSize             The size of the third dimension
 * \param lsize             The size of the fourth dimension
 */
void serialboxFrotranSerializerRegisterField(void* serializer, const char* name, int type,
                                             int bytesPerElement, int iSize, int jSize, int kSize,
                                             int lSize);

/*===------------------------------------------------------------------------------------------===*\
 *     FieldMetaInfo
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Add a meta-information `key=value` pair to `field` of the serializer
 *
 * This function corresponds to `fs_add_field_metainfo_i`
 *
 * \param serializer  Serializer to use
 * \param field       Name of the field
 * \param key         Key of the new element
 * \param value       Object to be copied to the value of the new element
 * @{
 */
void serialboxFortranSerializerAddFieldMetaInfoBoolean(void* serializer, const char* field,
                                                       const char* key, int value);
void serialboxFortranSerializerAddFieldMetaInfoInt32(void* serializer, const char* field,
                                                     const char* key, int value);
void serialboxFortranSerializerAddFieldMetaInfoFloat32(void* serializer, const char* field,
                                                       const char* key, float value);
void serialboxFortranSerializerAddFieldMetaInfoFloat64(void* serializer, const char* field,
                                                       const char* key, double value);
void serialboxFortranSerializerAddFieldMetaInfoString(void* serializer, const char* field,
                                                      const char* key, const char* value);
/** @} */

/*===------------------------------------------------------------------------------------------===*\
 *     Savepoint
\*===------------------------------------------------------------------------------------------===*/

/**
 * \brief Add a meta-information `key=value` pair to the `savepoint`
 *
 * This function corresponds to `fs_add_savepoint_metainfo_f`
 *
 * \param savepoint   Savepoint to use
 * \param key         Key of the new element
 * \param value       Object to be copied to the value of the new element
 * @{
 */
void serialboxFortranSavepointAddMetaInfoBoolean(void* savepoint, const char* key, int value);
void serialboxFortranSavepointAddMetaInfoInt32(void* savepoint, const char* key, int value);
void serialboxFortranSavepointAddMetaInfoFloat32(void* savepoint, const char* key, float value);
void serialboxFortranSavepointAddMetaInfoFloat64(void* savepoint, const char* key, double value);
void serialboxFortranSavepointAddMetaInfoString(void* savepoint, const char* key,
                                                const char* value);
/** @} */

/** @} @} */

#ifdef __cplusplus
}
#endif

#endif
