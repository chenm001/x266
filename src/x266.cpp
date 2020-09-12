/*****************************************************************************
 * x266.cpp: WC Encoder Functions
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

 /*****************************************************************************
 *****************************************************************************/
#define MAX_WIDTH           (4096)
#define MAX_HEIGHT          (2048)
#define REF_BLOCK_SZ        (16)
#define REF_FRAME_STRD      (MAX_WIDTH / REF_BLOCK_SZ)

// Thanks to https://gist.github.com/PhilCK/1534763
#ifdef __GNUC__
#define PACKED( class_to_pack ) class_to_pack __attribute__((__packed__))
#else
#define PACKED( class_to_pack ) __pragma( pack(push, 1) ) class_to_pack __pragma( pack(pop) )
#endif

/*****************************************************************************
 *****************************************************************************/

PACKED(struct _ref_block_t
{
    // REF_BLOCK_SZ=16
    uint8_t     m_Y[16*16];         // 256 bytes - Y
    uint8_t     m_C[2][8*8];        // 128 bytes - UV
    uint8_t     m_I[128];           // 128 bytes - Info
});
typedef struct _ref_block_t ref_block_t;

PACKED(struct _ref_frame_t
{
    ref_block_t     m_frame[(MAX_WIDTH/REF_BLOCK_SZ) * (MAX_HEIGHT/REF_BLOCK_SZ)];
});
typedef struct _ref_frame_t ref_frame_t;

typedef struct _codec_t
{
    ref_frame_t     m_frames[3];    // [0]=Cur, [1..N]=References

} codec_t;

/*****************************************************************************
 *****************************************************************************/
int main(int argc, char *argv[])
{
    if(argc < 5)
    {
        fprintf(stderr, "Usage: %s -i in_file -o out_file -w width -h height -f frames\n", argv[0]);
        return 0;
    }

    // Encoder parameters
    FILE *fpi = NULL;
    FILE *fpo = NULL;
    int nWidth = 0;
    int nHeight = 0;
    int nFrames = 0;

    int i;
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
        else if(!_stricmp(argv[i], "-w"))
        {
            nWidth = atoi(argv[++i]);
        }
        else if(!_stricmp(argv[i], "-h"))
        {
            nHeight = atoi(argv[++i]);
        }
        else if(!_stricmp(argv[i], "-f"))
        {
            nFrames = atoi(argv[++i]);
        }
    }

    // Validate parameters
    if(nWidth == 0 ||
       nHeight == 0 ||
       fpi == NULL ||
       fpo == NULL)
    {
        fprintf(stderr, "Parameters check failed\n");

        if(fpi)
            fclose(fpi);
        if(fpo)
            fclose(fpo);
        return -1;
    }

    // Encode

    // Cleanup
    fclose(fpi);
    fclose(fpo);

    return 0;
}

