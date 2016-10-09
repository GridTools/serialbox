/*===-- serialbox-c/Serializer.h ----------------------------------------------------*- C++ -*-===*\
 *
 *                                    S E R I A L B O X
 *
 * This file is distributed under terms of BSD license.
 * See LICENSE.txt for more information
 *
 *===------------------------------------------------------------------------------------------===//
 *
 *! \file
 *! This file contains the C implementation of the Serializer.
 *
\*===------------------------------------------------------------------------------------------===*/

#ifndef SERIALBOX_C_SERIALIZER_H
#define SERIALBOX_C_SERIALIZER_H

#include "serialbox-c/DataFieldInfo.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief Policy for opening files in the Serializer and Archive
 */
enum serialboxOpenModeKind { Read = 0, Write, Append };

/**
 * \brief Refrence to a serializer
 */
typedef void* serialboxSerializer_t;

/**
 * \brief Create a new Serializer
 *
 * \param mode         Mode of the Serializer
 * \param directory    Directory of the Archive and Serializer meta-data
 * \param prefix       Prefix of all filenames
 * \param archiveName  Name of Archive (e.g "BinaryArchive")
 *
 * \return Refrence to the newly constructed Serializer or NULL if an error occured
 *
 * This will read ´MetaData-prefix.json´ to initialize the Serializer and construct the Archive by
 * reading the ´ArchiveMetaData-prefix.json´.
 */
serialboxSerializer_t serialboxSerializerCreate(serialboxOpenModeKind mode, const char* directory,
                                                const char* prefix, const char* archive);

/**
 * \brief Destroy the serializer and deallocate all memory
 *
 * \param serializer  Serializer to use
 */
void serialboxSerializerDestroy(serialboxSerializer_t serializer);

/**
 * \brief Return mode of the Serializer
 *
 * \param serializer  Serializer to use
 * \return mode of the Serializer
 */
serialboxOpenModeKind serialboxSerializerGetMode(serialboxSerializer_t serializer);

/**
 * \brief Return the directory of the Serializer
 *
 * \param serializer  Serializer to use
 * \return directory of the Serializer as a null-terminated string
 */
const char* serialboxSerializerGetDirectory(serialboxSerializer_t serializer);

#ifdef __cplusplus
}
#endif

#endif
