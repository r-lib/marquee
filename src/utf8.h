#pragma once

#include <cstring>
#include <cstdint>
#include <vector>
#include <string>

/*
 Basic UTF-8 manipulation routines
 by Jeff Bezanson
 placed in the public domain Fall 2005

 This code is designed to provide the utilities you need to manipulate
 UTF-8 as an internal string encoding. These functions do not perform the
 error checking normally needed when handling UTF-8 data, so if you happen
 to be from the Unicode Consortium you will want to flay me alive.
 I do this because error checking can be performed at the boundaries (I/O),
 with these routines reserved for higher performance on data known to be
 valid.

 Source: https://www.cprogramming.com/tutorial/utf8.c

 Modified 2019 by Thomas Lin Pedersen to work with const char*
 */

static const uint32_t offsetsFromUTF8[6] = {
  0x00000000UL, 0x00003080UL, 0x000E2080UL,
  0x03C82080UL, 0xFA082080UL, 0x82082080UL
};

static const char trailingBytesForUTF8[256] = {
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

/* conversions without error checking
 only works for valid UTF-8, i.e. no 5- or 6-byte sequences
 srcsz = source size in bytes, or -1 if 0-terminated
 sz = dest size in # of wide characters

 returns # characters converted
 dest will always be L'\0'-terminated, even if there isn't enough room
 for all the characters.
 if sz = srcsz+1 (i.e. 4*srcsz+4 bytes), there will always be enough space.
 */
static int u8_toucs(uint32_t *dest, int sz, const char *src, int srcsz) {
  uint32_t ch;
  const char *src_end = src + srcsz;
  int nb;
  int i=0;

  while (i < sz-1) {
    nb = trailingBytesForUTF8[(unsigned char)*src];
    if (srcsz == -1) {
      if (*src == 0)
        goto done_toucs;
    }
    else {
      if (src + nb >= src_end)
        goto done_toucs;
    }
    ch = 0;
    switch (nb) {
    /* these fall through deliberately */
    case 5: ch += (unsigned char)*src++; ch <<= 6;
    case 4: ch += (unsigned char)*src++; ch <<= 6;
    case 3: ch += (unsigned char)*src++; ch <<= 6;
    case 2: ch += (unsigned char)*src++; ch <<= 6;
    case 1: ch += (unsigned char)*src++; ch <<= 6;
    case 0: ch += (unsigned char)*src++;
    }
    ch -= offsetsFromUTF8[nb];
    dest[i++] = ch;
  }
  done_toucs:
    dest[i] = 0;
  return i;
}
/* srcsz = number of source characters, or -1 if 0-terminated
 sz = size of dest buffer in bytes

 returns # characters converted
 dest will only be '\0'-terminated if there is enough space. this is
 for consistency; imagine there are 2 bytes of space left, but the next
 character requires 3 bytes. in this case we could NUL-terminate, but in
 general we can't when there's insufficient space. therefore this function
 only NUL-terminates if all the characters fit, and there's space for
 the NUL as well.
 the destination string will never be bigger than the source string.
 */
inline int u8_toutf8(char *dest, int sz, const uint32_t *src, int srcsz) {
  uint32_t ch;
  int i = 0;
  char *dest_end = dest + sz;

  while (srcsz<0 ? src[i]!=0 : i < srcsz) {
    ch = src[i];
    if (ch < 0x80) {
      if (dest >= dest_end)
        return i;
      *dest++ = (char)ch;
    }
    else if (ch < 0x800) {
      if (dest >= dest_end-1)
        return i;
      *dest++ = (ch>>6) | 0xC0;
      *dest++ = (ch & 0x3F) | 0x80;
    }
    else if (ch < 0x10000) {
      if (dest >= dest_end-2)
        return i;
      *dest++ = (ch>>12) | 0xE0;
      *dest++ = ((ch>>6) & 0x3F) | 0x80;
      *dest++ = (ch & 0x3F) | 0x80;
    }
    else if (ch < 0x110000) {
      if (dest >= dest_end-3)
        return i;
      *dest++ = (ch>>18) | 0xF0;
      *dest++ = ((ch>>12) & 0x3F) | 0x80;
      *dest++ = ((ch>>6) & 0x3F) | 0x80;
      *dest++ = (ch & 0x3F) | 0x80;
    }
    i++;
  }
  if (dest < dest_end)
    *dest = '\0';
  return i;
}
/*
 End of Basic UTF-8 manipulation routines
 by Jeff Bezanson
 */

class UTF_UCS {
  std::vector<uint32_t> buffer_ucs;
  std::vector<char> buffer_utf;

public:
  UTF_UCS() {
    // Allocate space in buffer
    buffer_ucs.resize(1024);
    buffer_utf.resize(1024, '\0');
  }
  ~UTF_UCS() {
  }
  const uint32_t * convert_to_ucs(const char * string, int &n_conv) {
    if (string == NULL) {
      n_conv = 0;
      return buffer_ucs.data();
    }
    int n_bytes = strlen(string) + 1;
    unsigned int max_size = n_bytes * 4;
    if (buffer_ucs.size() < max_size) {
      buffer_ucs.resize(max_size);
    }

    n_conv = u8_toucs(buffer_ucs.data(), max_size, string, -1);

    return buffer_ucs.data();
  }
  const char * convert_to_utf(const uint32_t * string, unsigned int str_len, int &n_conv) {
    if (string == NULL) {
      n_conv = 0;
      return buffer_utf.data();
    }
    unsigned int max_size = str_len * 4;
    if (buffer_utf.size() < max_size + 1) {
      buffer_utf.resize(max_size + 1, '\0');
    } else {
      buffer_utf[max_size] = '\0';
    }

    n_conv = u8_toutf8(buffer_utf.data(), max_size, string, str_len);

    return buffer_utf.data();
  }
};
