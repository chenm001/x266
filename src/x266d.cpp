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
#define BIT_BUF_SIZE                    (1 * 1024 * 1024)
#define REF_BLOCK_SZ                    (16)
#define MAX_VPS_LAYERS                  (1)     // maximum u(6)
#define MAX_VPS_SUB_LAYERS              (1)     // maximum u(3)
#define MAX_VPS_OUTPUT_LAYER            (2)     // maximum u(8)
#define MAX_VPS_PTLS                    (1)     // maximum u(8)
#define MAX_VPS_DPB_PARAMS              (1)     // maximum ue(v)
#define MAX_VPS_HRD_PARAMS              (1)     // maximum ue(v)

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
    #define CTZ(x)          __builtin_ctz(x)

#elif defined(_MSC_VER)
    #include <intrin.h>
    #define BSWAP32(x)      _byteswap_ulong(x)
    #define BSWAP64(x)      _byteswap_uint64(x)

    static uint32_t __inline CLZ(uint32_t x)
    {
       unsigned long r = 0;
       _BitScanReverse(&r, x);
       return r;
    }

    static uint32_t __inline CTZ(uint32_t x)
    {
       unsigned long r = 0;
       _BitScanForward(&r, x);
       return r;
    }
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
    uint64_t    dwCache;
    uint8_t    *pucBuf;
    int32_t     cachedBits;
    int32_t     rbspBits;
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

typedef struct _vps_t
{
    uint8_t     vps_video_parameter_set_id;                                             // u(4)
    uint8_t     vps_max_layers_minus1;                                                  // u(6)
    uint8_t     vps_max_sublayers_minus1;                                               // u(3)
    uint8_t     vps_default_ptl_dpb_hrd_max_tid_flag;                                   // u(1)
    uint8_t     vps_all_independent_layers_flag;                                        // u(1)
    uint8_t     vps_layer_id[MAX_VPS_LAYERS];                                           // []u(6)
    uint8_t     vps_independent_layer_flag[MAX_VPS_LAYERS];                             // []u(1)
    uint8_t     vps_max_tid_ref_present_flag[MAX_VPS_LAYERS];                           // []u(1)
    uint8_t     vps_direct_ref_layer_flag[MAX_VPS_LAYERS][MAX_VPS_LAYERS];              // [][]u(1)
    uint8_t     vps_max_tid_il_ref_pics_plus1[MAX_VPS_LAYERS][MAX_VPS_LAYERS];          // [][]u(3)
    uint8_t     vps_each_layer_is_an_ols_flag;                                          // u(1)
    uint8_t     vps_ols_mode_idc;                                                       // u(2)
    uint8_t     vps_num_output_layer_sets_minus2;                                       // u(8)
    uint8_t     vps_ols_output_layer_flag[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];  // [][]u(1)
    uint8_t     vps_num_ptls_minus1;                                                    // u(8)
    uint8_t     vps_pt_present_flag[MAX_VPS_PTLS];                                      // u(1)
    uint8_t     vps_ptl_max_tid[MAX_VPS_PTLS];                                          // u(3)
    uint8_t     vps_ols_ptl_idx[MAX_VPS_OUTPUT_LAYER];                                  // u(8)
    uint8_t     vps_num_dpb_params_minus1;                                              // ue(v)
    uint8_t     vps_sublayer_dpb_params_present_flag;                                   // u(1)
    uint8_t     vps_dpb_max_tid[MAX_VPS_DPB_PARAMS];                                    // u(3)
    uint16_t    vps_ols_dpb_pic_width[MAX_VPS_DPB_PARAMS];                              // ue(v)
    uint16_t    vps_ols_dpb_pic_height[MAX_VPS_DPB_PARAMS];                             // ue(v)
    uint8_t     vps_ols_dpb_chroma_format[MAX_VPS_DPB_PARAMS];                          // u(2)
    uint8_t     vps_ols_dpb_bitdepth_minus8[MAX_VPS_DPB_PARAMS];                        // ue(v)
    uint8_t     vps_ols_dpb_params_idx[MAX_VPS_DPB_PARAMS];                             // ue(v)
    uint8_t     vps_timing_hrd_params_present_flag;                                     // u(1)
    uint8_t     vps_sublayer_cpb_params_present_flag;                                   // u(1)
    uint8_t     vps_num_ols_timing_hrd_params_minus1;                                   // ue(v)
    uint8_t     vps_hrd_max_tid[MAX_VPS_DPB_PARAMS];                                    // u(3)
    uint8_t     vps_ols_timing_hrd_idx[MAX_VPS_OUTPUT_LAYER];                           // ue(v)
} vps_t;

typedef struct _codec_t
{
    param_t                 params;
    ref_block_t            *m_frames[3];            // [0]=Cur, [1..N]=References
    intptr_t                m_frames_strd;
    bs_t                    bs;
    nal_unit_header_t       nalu_header;
    vps_t                   vps;
} codec_t;

/*****************************************************************************
 *****************************************************************************/
// NOTE: There may read beyond boundary up to 3 bytes at end of stream!
void bsInit(bs_t *bs, uint8_t *buf, intptr_t size)
{
    assert(size >= 8 && "bitstream size too small");
    assert(!((uint32_t)buf & 7) && "bitstream must be aligned to 64 bits");

    bs->dwCache     = BSWAP64(AV64(buf));
    bs->cachedBits  = 8*sizeof(uint64_t);
    bs->pucBuf      = buf + sizeof(uint64_t);;

    // Remove trailing zero filling bytes
    while(size > 0 && !buf[size-1])
        size--;

    bs->rbspBits = (size << 3) - 8 + 7 - CTZ(buf[size - 1]);
}

uint32_t bsNextBits32(bs_t *bs)
{
    const uint32_t bins = (uint32_t)(bs->dwCache >> 32);
    return bins;
}

uint32_t bsNextBit(bs_t *bs)
{
    const int shift = 64 - 1;
    const uint32_t bins = (uint32_t)(bs->dwCache >> shift);
    return bins;
}

void bsSkipBit(bs_t *bs)
{
    bs->dwCache <<= 1;
    bs->cachedBits -= 1;
    bs->rbspBits--;

    const int left = bs->cachedBits;

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
    bs->dwCache <<= nBits;
    bs->cachedBits -= nBits;
    bs->rbspBits -= nBits;

    const int left = bs->cachedBits;

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

uint32_t bsGetBit(bs_t *bs)
{
    const uint32_t bins = bsNextBit(bs);
    bsSkipBit(bs);
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

int bsRbspRemain(bs_t *bs)
{
    return bs->rbspBits;
}

void bsRbspTrailingBits(bs_t *bs)
{
    assert(bs->rbspBits == 0 && "rbsp_trailing_bits check failed");
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

void general_constraints_info(codec_t *codec, bs_t *bs)
{
    // TODO:
}

void profile_tier_level(codec_t *codec, bs_t *bs, int profileTierPresentFlag, int MaxNumSubLayersMinus1)
{
    int i;

    if(profileTierPresentFlag)
    {
        int general_profile_idc = bsGetBits(bs, 7);
        int general_tier_flag = bsGetBit(bs);
    }
    int general_level_idx = bsGetBits(bs, 8);
    int ptl_frame_only_constraint_flag = bsGetBit(bs);
    int ptl_multilayer_enabled_flag = bsGetBit(bs);

    if(profileTierPresentFlag)
    {
        general_constraints_info(codec, bs);
    }

    int ptl_sublayer_level_present_flag[MAX_VPS_OUTPUT_LAYER];
    for(i = MaxNumSubLayersMinus1 - 1; i >= 0; i--)
    {
        ptl_sublayer_level_present_flag[i] = bsGetBit(bs);
    }

    bsSkipToByteAlign(bs);

    for(i = MaxNumSubLayersMinus1 - 1; i >= 0; i--)
    {
        if(ptl_sublayer_level_present_flag[i])
        {
            /*sublayer_level_idc[i] =*/ bsGetBits(bs, 8);
        }
    }
    if(profileTierPresentFlag)
    {
        int ptl_num_sub_profiles = bsGetBits(bs, 8);
        for(i = 0; i < ptl_num_sub_profiles; i++)
        {
            /*general_sub_profile_idc[i] =*/ bsGetBits(bs, 32);
        }
    }
}

void dpb_parameters(codec_t *codec, bs_t *bs)
{
    // TODO:
}

void general_timing_hrd_parameters(codec_t *codec, bs_t *bs)
{
    // TODO:
}

void ols_timing_hrd_parameters(codec_t *codec, bs_t *bs, int firstSubLayer, int hrd_max_tid)
{
    // TODO:
}

void xDecodeVPS(codec_t *codec, bs_t *bs)
{
    int i, j, k;
    vps_t *const vps = &codec->vps;

    memset(vps, 0, sizeof(vps_t));

    vps->vps_video_parameter_set_id = bsGetBits(bs, 4);
    vps->vps_max_layers_minus1 = bsGetBits(bs, 6);
    vps->vps_max_sublayers_minus1 = bsGetBits(bs, 3);

    assert(vps->vps_max_layers_minus1 + 1 <= MAX_VPS_LAYERS && "vps_max_layers_minus1 check failed");
    assert(vps->vps_max_sublayers_minus1 + 1 <= MAX_VPS_SUB_LAYERS && "vps_max_sublayers_minus1 check failed");

    if(vps->vps_max_layers_minus1 > 0)
    {
        if(vps->vps_max_sublayers_minus1 > 0)
        {
            vps->vps_default_ptl_dpb_hrd_max_tid_flag = bsGetBit(bs);
        }
        vps->vps_all_independent_layers_flag = bsGetBit(bs);
    }

    for(i = 0; i <= (int)vps->vps_max_layers_minus1; i++)
    {
        vps->vps_layer_id[i] = bsGetBits(bs, 6);
        if(i > 0 && !vps->vps_all_independent_layers_flag)
        {
            vps->vps_independent_layer_flag[i] = bsGetBit(bs);

            if(!vps->vps_independent_layer_flag[i])
            {
                uint32_t vps_max_tid_ref_present_flag;
                vps_max_tid_ref_present_flag = vps->vps_max_tid_ref_present_flag[i] = bsGetBit(bs);
                for(j = 0; j < i; j++)
                {
                    vps->vps_direct_ref_layer_flag[i][j] = bsGetBit(bs);
                    if(vps_max_tid_ref_present_flag && vps->vps_direct_ref_layer_flag[i][j])
                    {
                        vps->vps_max_tid_il_ref_pics_plus1[i][j] = bsGetBits(bs, 3);
                    }
                }
            }
        }
    }

    // Calculate NumDirectRefLayers[], ReferenceLayerIdx[][]
    uint8_t NumRefLayers[MAX_VPS_OUTPUT_LAYER] = {0};
    uint8_t ReferenceLayerIdx[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER] = {0};
    {
        uint8_t NumDirectRefLayers[MAX_VPS_OUTPUT_LAYER];
        uint8_t dependencyFlag[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];
        uint8_t LayerUsedAsRefLayerFlag[MAX_VPS_OUTPUT_LAYER];
        uint8_t DirectRefLayerIdx[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];

        for(i = 0; i <= vps->vps_max_layers_minus1; i++)
        {
            for(j = 0; j <= vps->vps_max_layers_minus1; j++)
            {
                dependencyFlag[i][j] = vps->vps_direct_ref_layer_flag[i][j];
                for(k = 0; k < i; k++)
                {
                    if(vps->vps_direct_ref_layer_flag[i][k] && dependencyFlag[k][j])
                        dependencyFlag[ i ][ j ] = 1;
                }
            }
            LayerUsedAsRefLayerFlag[i] = 0;
        }
        for(i = 0; i <= vps->vps_max_layers_minus1; i++)
        {
            int d, r;
            for(j = 0, d = 0, r = 0; j <= vps->vps_max_layers_minus1; j++)
            {
                if(vps->vps_direct_ref_layer_flag[i][j])
                {
                    DirectRefLayerIdx[i][d++] = j;
                    LayerUsedAsRefLayerFlag[j] = 1;
                }
                if(dependencyFlag[i][j])
                    ReferenceLayerIdx[i][r++] = j;
            }
            NumDirectRefLayers[i] = d;
            NumRefLayers[i] = r;
        }
    }

    vps->vps_ols_mode_idc = 4;
    if(vps->vps_max_layers_minus1 > 0)
    {
        if(vps->vps_all_independent_layers_flag)
        {
            vps->vps_each_layer_is_an_ols_flag = bsGetBit(bs);
        }
        if(!vps->vps_each_layer_is_an_ols_flag)
        {
            if(!vps->vps_all_independent_layers_flag)
            {
                vps->vps_ols_mode_idc = bsGetBits(bs, 2);
                assert(vps->vps_ols_mode_idc <= 2 && "vps_ols_mode_idc must be in range [0,2]\n");
            }
            if (vps->vps_ols_mode_idc == 2)
            {
                vps->vps_num_output_layer_sets_minus2 = bsGetBits(bs, 8);

                for (i = 0; i <= vps->vps_num_output_layer_sets_minus2 + 1; i++)
                {
                    for (j = 0; j <= vps->vps_max_layers_minus1; j++)
                    {
                        vps->vps_ols_output_layer_flag[i][j] = bsGetBit(bs);
                    }
                }
            }
        }
        vps->vps_num_ptls_minus1 = bsGetBits(bs, 8);
        assert(vps->vps_num_ptls_minus1+1 <= MAX_VPS_PTLS && "vps_num_ptls_minus1 check failed");
    }

    for(i = 0; i <= vps->vps_num_ptls_minus1; i++)
    {
        if(i > 0)
        {
            vps->vps_pt_present_flag[i] = bsGetBit(bs);
        }
        if(!vps->vps_default_ptl_dpb_hrd_max_tid_flag)
        {
            vps->vps_ptl_max_tid[i] = bsGetBits(bs, 3);
        }
    }
    bsSkipToByteAlign(bs);

    for(i = 0; i <= vps->vps_num_ptls_minus1; i++)
    {
        profile_tier_level(codec, bs, vps->vps_pt_present_flag[i], vps->vps_ptl_max_tid[i]);
    }

    const int TotalNumOlss = (vps->vps_ols_mode_idc == 2) ? vps->vps_num_output_layer_sets_minus2+2
                                                          : vps->vps_max_layers_minus1+1;

    assert(TotalNumOlss < MAX_VPS_OUTPUT_LAYER && "TotalNumOlss check failed");

    for(i = 0; i < TotalNumOlss; i++)
    {
        if(vps->vps_num_ptls_minus1 > 0 &&
           vps->vps_num_ptls_minus1 + 1 != TotalNumOlss)
        {
            vps->vps_ols_ptl_idx[i] = bsGetBits(bs, 8);
        }
    }

    // Calculate layerIncludedInOlsFlag[][]
    uint8_t layerIncludedInOlsFlag[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER] = {0};
    uint8_t OutputLayerIdx[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER] = {0};
    {
        const int olsModeIdc = vps->vps_ols_mode_idc;

        uint8_t NumOutputLayersInOls[MAX_VPS_OUTPUT_LAYER];
        uint8_t OutputLayerIdInOls[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];
        uint8_t NumSubLayersInLayerInOLS[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];
        uint8_t LayerUsedAsOutputLayerFlag[MAX_VPS_OUTPUT_LAYER];
        int maxSublayerNeeded;
        int m;

        NumOutputLayersInOls[0] = 1;
        OutputLayerIdInOls[0][0] = vps->vps_layer_id[0];
        NumSubLayersInLayerInOLS[0][0] = vps->vps_ptl_max_tid[vps->vps_ols_ptl_idx[0]] + 1;
        LayerUsedAsOutputLayerFlag[0] = 1;

        for(i = 1; i <= vps->vps_max_layers_minus1; i++)
        {
            if(olsModeIdc != 2/*vps->vps_ols_mode_idc = = 4 || vps->vps_ols_mode_idc < 2*/)
                LayerUsedAsOutputLayerFlag[i] = 1;
            else/* if(vps->vps_ols_mode_idc == 2)*/
                LayerUsedAsOutputLayerFlag[i] = 0;
        }
        for(i = 1; i < TotalNumOlss; i++)
        {
            if(olsModeIdc == 4 || olsModeIdc == 0)
            {
                NumOutputLayersInOls[i] = 1;
                OutputLayerIdInOls[i][0] = vps->vps_layer_id[i];
                if(vps->vps_each_layer_is_an_ols_flag)
                    NumSubLayersInLayerInOLS[i][0] = vps->vps_ptl_max_tid[vps->vps_ols_ptl_idx[i]] + 1;
                else {
                    NumSubLayersInLayerInOLS[i][i] = vps->vps_ptl_max_tid[vps->vps_ols_ptl_idx[i]] + 1;
                    for(k = i - 1; k >= 0; k--)
                    {
                        NumSubLayersInLayerInOLS[i][k] = 0;
                        for(m = k + 1; m <= i; m++)
                        {
                            maxSublayerNeeded = MIN(NumSubLayersInLayerInOLS[i][m], vps->vps_max_tid_il_ref_pics_plus1[m][k]);
                            if(vps->vps_direct_ref_layer_flag[m][k] && NumSubLayersInLayerInOLS[ i ][ k ] < maxSublayerNeeded)
                                NumSubLayersInLayerInOLS[ i ][ k ] = maxSublayerNeeded;
                        }
                    }
                }
            }
            else if(olsModeIdc == 1)
            {
                NumOutputLayersInOls[i] = i + 1;
                for(j = 0; j < NumOutputLayersInOls[i]; j++)
                {
                    OutputLayerIdInOls[i][j] = vps->vps_layer_id[j];
                    NumSubLayersInLayerInOLS[i][j] = vps->vps_ptl_max_tid[vps->vps_ols_ptl_idx[i]] + 1;
                }
            }
            else/* if(olsModeIdc == 2)*/
            {
                for(j = 0; j <= vps->vps_max_layers_minus1; j++)
                {
                    layerIncludedInOlsFlag[i][j] = 0;
                    NumSubLayersInLayerInOLS[i][j] = 0;
                }

                int highestIncludedLayer = 0;
                for(k = 0, j = 0; k <= vps->vps_max_layers_minus1; k++)
                {
                    if(vps->vps_ols_output_layer_flag[i][k])
                    {
                        layerIncludedInOlsFlag[i][k] = 1;
                        highestIncludedLayer = k;
                        LayerUsedAsOutputLayerFlag[k] = 1;
                        OutputLayerIdx[i][j] = k;
                        OutputLayerIdInOls[i][j++] = vps->vps_layer_id[k];
                        NumSubLayersInLayerInOLS[ i ][ k ] = vps->vps_ptl_max_tid[vps->vps_ols_ptl_idx[i]] + 1;
                    }
                }
                NumOutputLayersInOls[i] = j;
                for(j = 0; j < NumOutputLayersInOls[i]; j++)
                {
                    int idx = OutputLayerIdx[i][j];
                    for( k = 0; k < NumRefLayers[idx]; k++ )
                    {
                        if (!layerIncludedInOlsFlag[i][ReferenceLayerIdx[idx][k]])
                            layerIncludedInOlsFlag[i][ReferenceLayerIdx[idx][k]] = 1;
                    }
                }
                for(k = highestIncludedLayer - 1; k >= 0; k--)
                {
                    if(layerIncludedInOlsFlag[i][k] && !vps->vps_ols_output_layer_flag[i][k])
                    {
                        for(m = k + 1; m <= highestIncludedLayer; m++)
                        {
                            maxSublayerNeeded = MIN(NumSubLayersInLayerInOLS[i][m], vps->vps_max_tid_il_ref_pics_plus1[m][k]);
                            if(vps->vps_direct_ref_layer_flag[m][k] &&
                               layerIncludedInOlsFlag[i][m] &&
                               NumSubLayersInLayerInOLS[i][k] < maxSublayerNeeded)
                            {
                                NumSubLayersInLayerInOLS[i][k] = maxSublayerNeeded;
                            }
                        }
                    }
                }
            } // end of olsModeIdc==2
        }
    } // end of Calculate layerIncludedInOlsFlag[][]

    if(!vps->vps_each_layer_is_an_ols_flag)
    {
        vps->vps_num_dpb_params_minus1 = bsGetUe(bs);
        assert(vps->vps_num_dpb_params_minus1+1 < MAX_VPS_DPB_PARAMS && "vps_num_dpb_params_minus1 check failed");
        const int VpsNumDpbParams = vps->vps_num_dpb_params_minus1 + 1;

        if(vps->vps_max_sublayers_minus1 > 0)
        {
            vps->vps_sublayer_cpb_params_present_flag = bsGetBit(bs);
        }

        for(i = 0; i < VpsNumDpbParams; i++)
        {
            if(!vps->vps_default_ptl_dpb_hrd_max_tid_flag)
            {
                vps->vps_dpb_max_tid[i] = bsGetBits(bs, 3);
            }
            dpb_parameters(codec, bs);
        }

        // Calculate NumMultiLayerOlss
        int NumMultiLayerOls = 0;
        {
            uint8_t NumLayerInOls[MAX_VPS_OUTPUT_LAYER];
            uint8_t LayerIdInOls[MAX_VPS_OUTPUT_LAYER][MAX_VPS_OUTPUT_LAYER];

            NumLayerInOls[0] = 1;
            LayerIdInOls[0][0] = vps->vps_layer_id[0];
            for(i = 1; i < TotalNumOlss; i++)
            {
                if(vps->vps_each_layer_is_an_ols_flag)
                {
                    NumLayerInOls[i] = 1;
                    LayerIdInOls[i][0] = vps->vps_layer_id[i];
                }
                else if(vps->vps_ols_mode_idc <= 1 /*vps->vps_ols_mode_idc == 0 || vps->vps_ols_mode_idc == 1*/)
                {
                    NumLayerInOls[i] = i + 1;
                    for(j = 0; j < NumLayerInOls[i]; j++)
                    {
                        LayerIdInOls[i][j] = vps->vps_layer_id[j];
                    }
                }
                else if(vps->vps_ols_mode_idc == 2)
                {
                    for(j = 0, k = 0; k <= vps->vps_max_layers_minus1; k++)
                    {
                        if(layerIncludedInOlsFlag[i][k])
                            LayerIdInOls[i][j++] = vps->vps_layer_id[k];
                    }
                    NumLayerInOls[i] = j;
                }
                if(NumLayerInOls[i] > 1)
                {
                    //MultiLayerOlsIdx[i] = NumMultiLayerOlss;
                    NumMultiLayerOls++;
                }
            }
        }

        for(i = 0; i < NumMultiLayerOls; i++)
        {
            vps->vps_ols_dpb_pic_width[i] = bsGetUe(bs);
            vps->vps_ols_dpb_pic_height[i] = bsGetUe(bs);
            vps->vps_ols_dpb_chroma_format[i] = bsGetBits(bs, 2);
            vps->vps_ols_dpb_bitdepth_minus8[i] = bsGetUe(bs);
            if(VpsNumDpbParams > 1 && VpsNumDpbParams != NumMultiLayerOls)
            {
                vps->vps_ols_dpb_params_idx[i] = bsGetUe(bs);
            }
        }
        vps->vps_timing_hrd_params_present_flag = bsGetBit(bs);
        if(vps->vps_timing_hrd_params_present_flag)
        {
            general_timing_hrd_parameters(codec, bs);
            if(vps->vps_max_sublayers_minus1 > 0)
            {
                vps->vps_sublayer_cpb_params_present_flag = bsGetBit(bs);
            }
            vps->vps_num_ols_timing_hrd_params_minus1 = bsGetUe(bs);
            for(i = 0; i <= vps->vps_num_ols_timing_hrd_params_minus1; i++)
            {
                if(!vps->vps_default_ptl_dpb_hrd_max_tid_flag)
                {
                    vps->vps_hrd_max_tid[i] = bsGetBits(bs, 3);
                    const int firstSubLayer = vps->vps_sublayer_cpb_params_present_flag ? 0 : vps->vps_hrd_max_tid[i];
                    ols_timing_hrd_parameters(codec, bs, firstSubLayer, vps->vps_hrd_max_tid[i]);
                }
            }
            if(vps->vps_num_ols_timing_hrd_params_minus1 > 0 &&
               vps->vps_num_ols_timing_hrd_params_minus1+1 != 0)
            {
                for(i = 0; i < 0; i++)
                {
                    vps->vps_ols_timing_hrd_idx[i] = bsGetUe(bs);
                }
            }
        }
    }

    const int vps_extension_flag = bsGetBit(bs);
    if(vps_extension_flag)
    {
        // TODO: Skip multiple of bits at once
        while(bsRbspRemain(bs) > 0)
            bsSkipBit(bs);
    }

    bsRbspTrailingBits(bs);
}

int xDecodeFrame(codec_t *codec, uint8_t *buf, int size)
{
    bs_t *bs = &codec->bs;
    bsInit(bs, buf, size);

    nal_unit_header(bs, &codec->nalu_header);

    switch(codec->nalu_header.nal_unit_type)
    {
    case VPS_NUT:
        break;

    case SPS_NUT:
        break;

    case PPS_NUT:
        break;

    default:
        fprintf(stderr, "Unsupport nal type %d\n", codec->nalu_header.nal_unit_type);
        return -1;
    }

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

        // Decode Frame (input NALU without Start Code)
        ret = xDecodeFrame(&codec, &bitBuf0[nPos0+4], nFrameSize);

        // Check decode error
        if(ret < 0)
            break;

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
