//===-- serialbox/Core/SHA256.cpp ---------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// Implementation of the SHA-1 cryptographic hash function.
///
/// License
///
/// Author:     Brad Conte (brad AT bradconte.com)
/// Disclaimer: This code is presented "as is" without any guarantees.
/// Details:    Defines the API for the corresponding SHA1 implementation.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/SHA256.h"
#include <cstdlib>
#include <cstring>
#include <sstream>

namespace serialbox {

namespace sha256 {

using byte_t = unsigned char; // 8-bit byte
using word_t = std::int32_t;  // 32-bit word, change to "long" for 16-bit machines

typedef struct {
  byte_t data[64];
  word_t datalen;
  unsigned long long bitlen;
  word_t state[8];
} ctx_t;

#define ROTLEFT(a, b) (((a) << (b)) | ((a) >> (32 - (b))))
#define ROTRIGHT(a, b) (((a) >> (b)) | ((a) << (32 - (b))))

#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTRIGHT(x, 2) ^ ROTRIGHT(x, 13) ^ ROTRIGHT(x, 22))
#define EP1(x) (ROTRIGHT(x, 6) ^ ROTRIGHT(x, 11) ^ ROTRIGHT(x, 25))
#define SIG0(x) (ROTRIGHT(x, 7) ^ ROTRIGHT(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTRIGHT(x, 17) ^ ROTRIGHT(x, 19) ^ ((x) >> 10))

static const word_t k[64] = {static_cast<word_t>(1116352408), static_cast<word_t>(1899447441),
                             static_cast<word_t>(3049323471), static_cast<word_t>(3921009573),
                             static_cast<word_t>(961987163),  static_cast<word_t>(1508970993),
                             static_cast<word_t>(2453635748), static_cast<word_t>(2870763221),
                             static_cast<word_t>(3624381080), static_cast<word_t>(310598401),
                             static_cast<word_t>(607225278),  static_cast<word_t>(1426881987),
                             static_cast<word_t>(1925078388), static_cast<word_t>(2162078206),
                             static_cast<word_t>(2614888103), static_cast<word_t>(3248222580),
                             static_cast<word_t>(3835390401), static_cast<word_t>(4022224774),
                             static_cast<word_t>(264347078),  static_cast<word_t>(604807628),
                             static_cast<word_t>(770255983),  static_cast<word_t>(1249150122),
                             static_cast<word_t>(1555081692), static_cast<word_t>(1996064986),
                             static_cast<word_t>(2554220882), static_cast<word_t>(2821834349),
                             static_cast<word_t>(2952996808), static_cast<word_t>(3210313671),
                             static_cast<word_t>(3336571891), static_cast<word_t>(3584528711),
                             static_cast<word_t>(113926993),  static_cast<word_t>(338241895),
                             static_cast<word_t>(666307205),  static_cast<word_t>(773529912),
                             static_cast<word_t>(1294757372), static_cast<word_t>(1396182291),
                             static_cast<word_t>(1695183700), static_cast<word_t>(1986661051),
                             static_cast<word_t>(2177026350), static_cast<word_t>(2456956037),
                             static_cast<word_t>(2730485921), static_cast<word_t>(2820302411),
                             static_cast<word_t>(3259730800), static_cast<word_t>(3345764771),
                             static_cast<word_t>(3516065817), static_cast<word_t>(3600352804),
                             static_cast<word_t>(4094571909), static_cast<word_t>(275423344),
                             static_cast<word_t>(430227734),  static_cast<word_t>(506948616),
                             static_cast<word_t>(659060556),  static_cast<word_t>(883997877),
                             static_cast<word_t>(958139571),  static_cast<word_t>(1322822218),
                             static_cast<word_t>(1537002063), static_cast<word_t>(1747873779),
                             static_cast<word_t>(1955562222), static_cast<word_t>(2024104815),
                             static_cast<word_t>(2227730452), static_cast<word_t>(2361852424),
                             static_cast<word_t>(2428436474), static_cast<word_t>(2756734187),
                             static_cast<word_t>(3204031479), static_cast<word_t>(3329325298)};

static inline void sha256_transform(ctx_t* ctx, const byte_t data[]) {
  word_t a, b, c, d, e, f, g, h, i, j, t1, t2, m[64];

  for(i = 0, j = 0; i < 16; ++i, j += 4)
    m[i] = (data[j] << 24) | (data[j + 1] << 16) | (data[j + 2] << 8) | (data[j + 3]);
  for(; i < 64; ++i)
    m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];

  a = ctx->state[0];
  b = ctx->state[1];
  c = ctx->state[2];
  d = ctx->state[3];
  e = ctx->state[4];
  f = ctx->state[5];
  g = ctx->state[6];
  h = ctx->state[7];

  for(i = 0; i < 64; ++i) {
    t1 = h + EP1(e) + CH(e, f, g) + k[i] + m[i];
    t2 = EP0(a) + MAJ(a, b, c);
    h = g;
    g = f;
    f = e;
    e = d + t1;
    d = c;
    c = b;
    b = a;
    a = t1 + t2;
  }

  ctx->state[0] += a;
  ctx->state[1] += b;
  ctx->state[2] += c;
  ctx->state[3] += d;
  ctx->state[4] += e;
  ctx->state[5] += f;
  ctx->state[6] += g;
  ctx->state[7] += h;
}

static void sha256_init(ctx_t* ctx) {
  ctx->datalen = 0;
  ctx->bitlen = 0;
  ctx->state[0] = 0x6a09e667;
  ctx->state[1] = 0xbb67ae85;
  ctx->state[2] = 0x3c6ef372;
  ctx->state[3] = 0xa54ff53a;
  ctx->state[4] = 0x510e527f;
  ctx->state[5] = 0x9b05688c;
  ctx->state[6] = 0x1f83d9ab;
  ctx->state[7] = 0x5be0cd19;
}

static void sha256_update(ctx_t* ctx, const byte_t data[], size_t len) {
  word_t i;

  for(i = 0; i < len; ++i) {
    ctx->data[ctx->datalen] = data[i];
    ctx->datalen++;
    if(ctx->datalen == 64) {
      sha256_transform(ctx, ctx->data);
      ctx->bitlen += 512;
      ctx->datalen = 0;
    }
  }
}

static void sha256_final(ctx_t* ctx, byte_t hash[32]) {
  word_t i;

  i = ctx->datalen;

  // Pad whatever data is left in the buffer.
  if(ctx->datalen < 56) {
    ctx->data[i++] = 0x80;
    while(i < 56)
      ctx->data[i++] = 0x00;
  } else {
    ctx->data[i++] = 0x80;
    while(i < 64)
      ctx->data[i++] = 0x00;
    sha256_transform(ctx, ctx->data);
    std::memset(ctx->data, 0, 56);
  }

  // Append to the padding the total message's length in bits and transform.
  ctx->bitlen += ctx->datalen * 8;
  ctx->data[63] = ctx->bitlen;
  ctx->data[62] = ctx->bitlen >> 8;
  ctx->data[61] = ctx->bitlen >> 16;
  ctx->data[60] = ctx->bitlen >> 24;
  ctx->data[59] = ctx->bitlen >> 32;
  ctx->data[58] = ctx->bitlen >> 40;
  ctx->data[57] = ctx->bitlen >> 48;
  ctx->data[56] = ctx->bitlen >> 56;
  sha256_transform(ctx, ctx->data);

  // Since this implementation uses little endian byte ordering and SHA uses big endian,
  // reverse all the bytes when copying the final state to the output hash.
  for(i = 0; i < 4; ++i) {
    hash[i] = (ctx->state[0] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 4] = (ctx->state[1] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 8] = (ctx->state[2] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 12] = (ctx->state[3] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 16] = (ctx->state[4] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 20] = (ctx->state[5] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 24] = (ctx->state[6] >> (24 - i * 8)) & 0x000000ff;
    hash[i + 28] = (ctx->state[7] >> (24 - i * 8)) & 0x000000ff;
  }
}

static void sha256(const void* data, int size, byte_t hash[32]) {
  ctx_t context;

  sha256_init(&context);
  sha256_update(&context, (const byte_t*)data, size);
  sha256_final(&context, hash);
}

} // sha256

const char* SHA256::Name = "SHA256";

std::string SHA256::hash(const void* data, int length) noexcept {
  sha256::byte_t hash[32];
  sha256::sha256(data, length, hash);

  std::ostringstream ss;
  for(int i = 0; i < 32; ++i)
    ss << std::hex << std::uppercase << static_cast<int>(hash[i]);

  return ss.str();
}

} // namespace serialbox
