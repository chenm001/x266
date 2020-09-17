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

PACKED(struct _bitStream_t
{
    uint8_t    *pucBits;
    uint8_t    *pucBits0;
    uint64_t    dwCache;
    int32_t     nCachedBits;
});
typedef _bitStream_t bitStream_t;

enum
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

typedef struct _codec_t
{
    param_t         params;
    ref_block_t    *m_frames[3];            // [0]=Cur, [1..N]=References
    intptr_t        m_frames_strd;
    bitStream_t     bitstrm;
} codec_t;

/*****************************************************************************
 *****************************************************************************/
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
    uint8_t *bitBuf = NULL;

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
    bitBuf = (uint8_t*)_aligned_malloc(BIT_BUF_SIZE * sizeof(uint8_t), 64);
    assert(bitBuf && "bitBuf Memory allocate failed\n");

    if(fpi == NULL ||
       fpo == NULL)
    {
        fprintf(stderr, "FILE open failed\n");
        goto _cleanup;
    }

_cleanup:;
    if(fpi)
        fclose(fpi);
    if(fpo)
        fclose(fpo);
    if(bitBuf)
        _aligned_free(bitBuf);

    return ret;
}
