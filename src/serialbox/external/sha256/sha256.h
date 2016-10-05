/*********************************************************************
* Filename:   sha256.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA1 implementation.
*********************************************************************/

#ifndef SHA256_H
#define SHA256_H

#include <stddef.h>

typedef unsigned char BYTE; // 8-bit byte

#ifdef __cplusplus
extern "C" {
#endif

void sha256(const void* data, int length, BYTE hash[32]);

#ifdef __cplusplus
}
#endif

#endif   // SHA256_H

