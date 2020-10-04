/*****************************************************************************
 * x266d.cpp: WC Decoder
 *****************************************************************************
 * Copyright (C) 2015-2020 x266 project
 *
 * Authors: Min Chen <chenm003@163.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111, USA.
 *
 * This program is also available under a commercial proprietary license.
 * For more information, contact us at chenm003@163.com.
 *****************************************************************************/

#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

/*****************************************************************************
 *****************************************************************************/
#define BIT_BUF_SIZE        (1 * 1024 * 1024)
#define REF_BLOCK_SZ        (16)

// Thanks to https://gist.github.com/PhilCK/1534763
#ifdef __GNUC__
#define PACKED( class_to_pack ) class_to_pack __attribute__((__packed__))
#else
#define PACKED( class_to_pack ) __pragma( pack(push, 1) ) class_to_pack __pragma( pack(pop) )
#endif

#define ASIZE(x)            (sizeof(x) / sizeof((x)[0]))
#define xSHR(x, n)          ( (n)>=32 ? 0 : ((x)>>(n)) )
#define xSHL(x, n)          ( (n)>=32 ? 0 : ((x)<<(n)) )
#define MAX(a, b)           ( (a) > (b) ? (a) : (b) )
#define MIN(a, b)           ( (a) < (b) ? (a) : (b) )
#define AV32(a)             (*(uint32_t*)(a))
#define AV64(a)             (*(uint64_t*)(a))

#if defined(__GNUC__)
    #define BSWAP32(x)      __builtin_bswap32(x)
    #define BSWAP64(x)      __builtin_bswap64(x)
    #define CLZ(x)          (__builtin_clz(x) ^ 31)
#elif defined(_MSC_VER)
    #include <intrin.h>
    #define BSWAP32(x)      _byteswap_ulong(x)
    #define BSWAP64(x)      _byteswap_uint64(x)
    #define CLZ(x)          __lzcnt(x)
#else
    #error Unsupport platform
#endif

/*****************************************************************************
 *****************************************************************************/

PACKED(struct _ref_block_t
{
    // REF_BLOCK_SZ=16
    uint8_t     m_Y[16*16];         // 256 bytes - Y
    uint8_t     m_C[2*8*8];         // 128 bytes - UV
    uint8_t     m_I[128];           // 128 bytes - Info
});
typedef struct _ref_block_t ref_block_t;

PACKED(struct _param_t
{
    uint32_t nWidth;
    uint32_t nHeight;
});
typedef struct _param_t param_t;

PACKED(struct _bs_t
{
    uint8_t    *pucBuf;
    uint8_t    *pucBufEnd;
    uint64_t    dwCache;
    int32_t     cachedBits;
});
typedef _bs_t bs_t;

enum _nal_unit_type
{
    TRAIL_NUT = 0,
    STSA_NUT = 1,
    RADL_NUT = 2,
    RASL_NUT = 3,
    IDR_W_RADL = 7,
    IDR_N_LP = 8,
    CRA_NUT = 9,
    VPS_NUT = 14,
    SPS_NUT = 15,
    PPS_NUT = 16,
    PH_NUT = 19,
};

PACKED(struct _nal_unit_header_t
{
    uint8_t     nuh_layer_id;
    uint8_t     nal_unit_type;
});
typedef _nal_unit_header_t nal_unit_header_t;

typedef struct _codec_t
{
    param_t                 params;
    ref_block_t            *m_frames[3];            // [0]=Cur, [1..N]=References
    intptr_t                m_frames_strd;
    bs_t                    bs;
    nal_unit_header_t       nalu_header;
} codec_t;

/*****************************************************************************
 *****************************************************************************/
// NOTE: There may read beyond boundary up to 3 bytes at end of stream!
void bsInit(bs_t *bs, uint8_t *buf, intptr_t size)
{
    assert(size >= 8 && "bitstream size too small");
    assert(!((uint32_t)bs & 7) && "bitstream must be aligned to 64 bits");

    bs->dwCache     = BSWAP64(AV64(buf));
    bs->cachedBits = 8*sizeof(uint64_t);
    bs->pucBuf      = buf + sizeof(uint64_t);;
    bs->pucBufEnd   = buf + size;
}

uint32_t bsNextBits32(bs_t *bs)
{
    const uint32_t bins = (uint32_t)(bs->dwCache >> 32);
    return bins;
}

uint32_t bsNextBits(bs_t *bs, int nBits)
{
    assert(nBits <= 32 && "nBits must be less or equal to 32");
    const int shift = 64 - nBits;
    const uint32_t bins = (uint32_t)(bs->dwCache >> shift);
    return bins;
}

void bsSkipBits(bs_t *bs, int nBits)
{
    assert(nBits <= 32 && "nBits must be less or equal to 32");
    const int left = bs->cachedBits - nBits;
    bs->dwCache <<= nBits;
    bs->cachedBits -= nBits;

    // Refill
    if(left <= 32)
    {
        const int shift = 64 - left - 32;
        const uint32_t val = BSWAP32(AV32(bs->pucBuf));
        bs->pucBuf += sizeof(uint32_t);
        bs->cachedBits += 8*sizeof(uint32_t);
        bs->dwCache |= ((uint64_t)val << shift);
    }
}

uint32_t bsGetBits(bs_t *bs, int nBits)
{
    const uint32_t bins = bsNextBits(bs, nBits);
    bsSkipBits(bs, nBits);
    return bins;
}

uint32_t bsGetUe(bs_t *bs)
{
    const uint32_t tmp = 0x80000000;//bsNextBits32(bs);
    const int clz = 31 - CLZ(tmp);
    return bsGetBits(bs, 2*clz+1) - 1;
}

int32_t bsGetSe(bs_t *bs)
{
    const uint32_t tmp = 0x80000000;//bsNextBits32(bs);
    const int clz = 31 - CLZ(tmp);
    const uint32_t bins = bsGetBits(bs, 2*clz+1);
    int32_t val = (bins >> 1);
    if(bins & 1)
        val = - val;
    return val;
}

void bsSkipToByteAlign(bs_t *bs)
{
    const uint32_t bins = 8 - (bs->cachedBits & 7);
    bsSkipBits(bs, bins);
}

int bsBitsRemain(bs_t *bs)
{
    // NOTE: may negative number
    const int remain = ((uint8_t*)bs->pucBufEnd - (uint8_t*)bs->pucBuf) * 8;
    return (remain + bs->cachedBits);
}

int bsFindStartCodeAndEmulation(uint8_t *inBuf, int size, uint8_t *outBuf, uint32_t *outSize)
{
    int i;
    uint32_t tmp = ~0;
    uint32_t pos = 0;

    if(size < 4)
        return -1;

    for(i = 0; i < size; i++)
    {
        const uint8_t val = inBuf[i];
        tmp = (tmp << 8) | val;
        if(tmp == 0x00000001)
            break;
        if((tmp & 0x00FFFFFF) != 3)
            outBuf[pos++] = val;
    }

    *outSize = pos-3;

    return (tmp == 0x00000001 ? (i-3) : -1);
}


void xConvInputFmt(ref_block_t     *pBlock,
                   const uint8_t   *inpY,
                   const uint8_t   *inpU,
                   const uint8_t   *inpV,
                   const intptr_t   strdY,
                   const int        width,
                   const int        height)
{
    assert(!(width % REF_BLOCK_SZ) && "width is not multiple of 16");
    assert(!(height % REF_BLOCK_SZ) && "height is not multiple of 16");

    const intptr_t strdC = (strdY >> 1);

    int i, j, x, y;
    for(y = 0; y < height; y += REF_BLOCK_SZ)
    {
        for(x = 0; x < width; x += REF_BLOCK_SZ)
        {
            // Y
            for(i = 0; i < REF_BLOCK_SZ; i++)
            {
                memcpy(&pBlock->m_Y[i * REF_BLOCK_SZ],
                       &inpY[(y+i)*strdY + x],
                       REF_BLOCK_SZ*sizeof(uint8_t));
            }

            // UV - 420SP
            for(i = 0; i < REF_BLOCK_SZ/2; i++)
            {
                for(j = 0; j < REF_BLOCK_SZ/2; j++)
                {
                    pBlock->m_C[i*REF_BLOCK_SZ + j*2 + 0] = inpU[((y>>1)+i)*strdC + ((x>>1)+j)];
                    pBlock->m_C[i*REF_BLOCK_SZ + j*2 + 1] = inpV[((y>>1)+i)*strdC + ((x>>1)+j)];
                }
            }
            pBlock++;
        }
    }
}

void xConvOutput420(const ref_block_t  *pBlock,
                    uint8_t            *outY,
                    const intptr_t      strdY,
                    uint8_t            *outU,
                    uint8_t            *outV,
                    intptr_t            strdC,
                    const int           width,
                    const int           height)
{
    assert(!(width % REF_BLOCK_SZ) && "width is not multiple of 16");
    assert(!(height % REF_BLOCK_SZ) && "height is not multiple of 16");

    int i, j, x, y;
    for(y = 0; y < height; y += REF_BLOCK_SZ)
    {
        for(x = 0; x < width; x += REF_BLOCK_SZ)
        {
            // Y
            for(i = 0; i < REF_BLOCK_SZ; i++)
            {
                memcpy(&outY[(y+i)*strdY + x],
                       &pBlock->m_Y[i * REF_BLOCK_SZ],
                       REF_BLOCK_SZ*sizeof(uint8_t));
            }

            // UV - 420SP
            for(i = 0; i < REF_BLOCK_SZ/2; i++)
            {
                for(j = 0; j < REF_BLOCK_SZ/2; j++)
                {
                    outU[((y>>1)+i)*strdC + ((x>>1)+j)] = pBlock->m_C[i*REF_BLOCK_SZ + j*2 + 0];
                    outV[((y>>1)+i)*strdC + ((x>>1)+j)] = pBlock->m_C[i*REF_BLOCK_SZ + j*2 + 1];
                }
            }
            pBlock++;
        }
    }
}

/*****************************************************************************
 * Parsing Functions (JVET-S2001-vH)
 *****************************************************************************/
void nal_unit_header(bs_t *bs, nal_unit_header_t *nuh)
{
    uint32_t val = bsGetBits(bs, 16);

    nuh->nuh_layer_id = (val >> 8) & 0x3F;
    nuh->nal_unit_type = (val >> 3) & 0x1F;
}

/*****************************************************************************
 * Decode Framework
 *****************************************************************************/
void xDecodeInit(codec_t *codec)
{
    memset(codec, 0, sizeof(*codec));
}

int xDecodeFrame(codec_t *codec, uint8_t *buf, int size)
{
    bs_t *bs = &codec->bs;
    bsInit(bs, buf, size);

    nal_unit_header(bs, &codec->nalu_header);

    return 0;
}
 
 /*****************************************************************************
 *****************************************************************************/
int main(int argc, char *argv[])
{
    if(argc < 2)
    {
        fprintf(stderr, "Usage: %s -i in_bit -o out_yuv -f frames\n", argv[0]);
        return 0;
    }

    int i;

    // Interface
    int ret = 0;

    // Parameters
    FILE *fpi = NULL;
    FILE *fpo = NULL;
    uint32_t nFrames = -1;

    // Internal
    uint8_t *bitBuf0 = NULL;
    uint8_t *bitBuf1 = NULL;
    uint32_t bitSize0 = 0;
    uint32_t bitSize1 = 0;

    for(i = 1; i < argc; i++)
    {
        if(!_stricmp(argv[i], "-i"))
        {
            fpi = fopen(argv[++i], "rb");
        }
        else if(!_stricmp(argv[i], "-o"))
        {
            fpo = fopen(argv[++i], "wb");
        }
        else if(!_stricmp(argv[i], "-f"))
        {
            nFrames = atoi(argv[++i]);
        }
    }

    // Prepare
    bitBuf0 = (uint8_t*)_aligned_malloc(BIT_BUF_SIZE * sizeof(uint8_t), 64);
    assert(bitBuf0 && "bitBuf Memory allocate failed\n");

    bitBuf1 = (uint8_t*)_aligned_malloc(BIT_BUF_SIZE * sizeof(uint8_t), 64);
    assert(bitBuf1 && "bitBuf Memory allocate failed\n");

    if(fpi == NULL ||
       fpo == NULL)
    {
        fprintf(stderr, "FILE open failed\n");
        goto _cleanup;
    }

    // Decode
    codec_t codec;

    bitSize0 = fread(bitBuf0, 1, BIT_BUF_SIZE, fpi);

    int nPos0 = bsFindStartCodeAndEmulation(bitBuf0, bitSize0, bitBuf1, &bitSize1);
    assert(nPos0 >= 0 && "First Start Code does not found in the stream");

    // Drop leading garbage bytes
    bitSize0 -= nPos0;

    xDecodeInit(&codec);

    // Decode loop
    while(1)
    {
        int nPos1 = bsFindStartCodeAndEmulation(&bitBuf0[nPos0+5], bitSize0, bitBuf1, &bitSize1);

        if(nPos1 < 0)
        {
            // End of stream
            if(feof(fpi) ||
               (bitSize0 == BIT_BUF_SIZE))
                break;

            // Move data to front of buffer
            memmove(&bitBuf0[0], &bitBuf0[nPos0], bitSize0);
            nPos0 = 0;

            // Refill
            bitSize0 += fread(&bitBuf0[bitSize0], 1, BIT_BUF_SIZE-bitSize0, fpi);
            continue;
        }

        // NALU size
        int nFrameSize = nPos1 - nPos0;

        // Remove trailing zero filling bytes
        while(!bitBuf0[nPos0 + nFrameSize - 1])
            nFrameSize--;

        // Decode Frame (input NALU without Start Code)
        ret = xDecodeFrame(&codec, &bitBuf0[nPos0+4], nFrameSize);

        // Loop Next
        nPos0 = nPos1;
        bitSize0 -= nFrameSize;
    }

    // Decode Flush

_cleanup:;
    if(fpi)
        fclose(fpi);
    if(fpo)
        fclose(fpo);
    if(bitBuf0)
        _aligned_free(bitBuf0);
    if(bitBuf1)
        _aligned_free(bitBuf1);

    return ret;
}
