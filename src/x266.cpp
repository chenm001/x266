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
#include <assert.h>

 /*****************************************************************************
 *****************************************************************************/
#define MAX_WIDTH           (4096)
#define MAX_HEIGHT          (2048)
#define REF_BLOCK_SZ        (16)
#define REF_FRAME_STRD      (MAX_WIDTH / REF_BLOCK_SZ)
#define BIT_BUF_SIZE        (1 * 1024 * 1024)

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
    uint32_t    dwCache;
    int32_t     nCachedBits;
    uint8_t    *pucBits;
    uint8_t    *pucBits0;
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
// Credit to my x265 @ https://code.google.com/archive/p/x265/
// TODO: bsr
static int xLog2(uint32_t x)
{
    uint32_t log2Size = 0;
    while(x > 0) {
        x >>= 1;
        log2Size++;
    }
    return(log2Size);
}

#define putBits32(dst, x)    *(uint32_t*)(dst) = (x);
#define BSWAP32(x)          ( (x<<24) + ((x<<8)&0xff0000) + ((x>>8)&0xff00) + (x>>24) )
#define flushCache(dst, x, bits)    \
{ \
    int _i; \
    for(_i=0; _i < (bits)>>3; _i++) \
    { \
        const uint32_t _tmp = (x) >> 24; \
        (x) <<= 8; \
        if (   (dst)[-1] == 0 \
            && (dst)[-2] == 0 \
            && _tmp <= 3 ) \
            *(dst)++ = 0x03; \
        *(dst)++ = _tmp; \
    } \
}

// ***************************************************************************
static void xBitStreamInit(bitStream_t *pBS, uint8_t *pucBuffer, int nBufferSize)
{
    assert( nBufferSize > 0 );

    pBS->pucBits        = pucBuffer;
    pBS->pucBits0       = pucBuffer;
    pBS->dwCache        = 0;
    pBS->nCachedBits    = 0;
}

static void xPutBits32(bitStream_t *pBS, uint32_t uiBits)
{
    assert( pBS->nCachedBits % 8 == 0 );
    putBits32(pBS->pucBits, uiBits);
    pBS->pucBits += 4;
}

static void xPutBits(bitStream_t *pBS, uint32_t uiBits, int nNumBits)
{
    int nShift = 32 - pBS->nCachedBits - nNumBits;
    
    assert((nNumBits >= 0) && (nNumBits <= 32));
    assert(xSHR(uiBits, nNumBits) == 0);

    if (nShift >= 0)
    {
        pBS->dwCache     |= xSHL(uiBits, nShift);
        pBS->nCachedBits += nNumBits;
    }
    else
    {
        uint32_t dwCache = pBS->dwCache;
        dwCache |= xSHR(uiBits, -nShift);

        flushCache(pBS->pucBits, dwCache, 32);
        
        pBS->dwCache = xSHL(uiBits, (32 + nShift));
        pBS->nCachedBits = -nShift;
    }
}

static void xWriteAlignZero(bitStream_t *pBS)
{
    int nShift = (32 - pBS->nCachedBits) & 7;
    xPutBits(pBS, 0, nShift);
}

static void xWriteAlignOne(bitStream_t *pBS)
{
    int nShift = (32 - pBS->nCachedBits) & 7;
    xPutBits(pBS, (1<<nShift)-1, nShift);
}

static void xWriteRBSPTrailingBits(bitStream_t *pBS)
{
    xPutBits(pBS, 1, 1);
    xWriteAlignZero(pBS);
}

static int32_t xBitFlush(bitStream_t *pBS)
{
    flushCache(pBS->pucBits, pBS->dwCache, pBS->nCachedBits);
    pBS->nCachedBits &= 7;
    return (pBS->pucBits - pBS->pucBits0) + ((pBS->nCachedBits + 7) >> 3);
}

static void xWriteUvlc(bitStream_t *pBS, uint32_t uiCode)
{
    uint32_t uiLength = xLog2(++uiCode) - 1;

    xPutBits( pBS, 0,       uiLength);
    xPutBits( pBS, uiCode, (uiLength+1));
}

static uint32_t xConvertToUInt(int iValue)
{
    return ( iValue > 0) ? (iValue<<1)-1 : -iValue<<1;
}

static void xWriteSvlc( bitStream_t *pBS, int32_t iCode )
{
    uint32_t uiCode;
  
    uiCode = xConvertToUInt( iCode );
    xWriteUvlc( pBS, uiCode );
}

static void codeConstraintInfo(codec_t *codec, bitStream_t *bitstrm)
{
    xPutBits(bitstrm, 0, 1);            // gci_present_flag
    xWriteAlignZero(bitstrm);           // gci_alignment_zero_bit
}

static void codeProfileTierLevel(codec_t *codec, bitStream_t *bitstrm)
{
    xPutBits(bitstrm, 0, 7);            // general_profile_idc
    xPutBits(bitstrm, 0, 1);            // general_tier_flag
    xPutBits(bitstrm, 0, 8);            // general_level_idc
    xPutBits(bitstrm, 1, 1);            // ptl_frame_only_constraint_flag
    xPutBits(bitstrm, 0, 1);            // ptl_multilayer_enabled_flag
    codeConstraintInfo(codec, bitstrm);
    xWriteAlignZero(bitstrm);           // ptl_reserved_zero_bit
    xPutBits(bitstrm, 0, 8);            // ptl_num_sub_profiles
}

void xWriteNALHeader(bitStream_t *bitstrm, const int nalType)
{
    const uint32_t val = (nalType << 3) | 1;    // [0, 0, 000000, ttttt, 001]
    xPutBits32(bitstrm, BSWAP32(1));            // Start Code
    xPutBits(bitstrm, val, 16);
}

void xWriteSPS(codec_t *codec)
{
    bitStream_t *bitstrm = &codec->bitstrm;

    xWriteNALHeader(bitstrm, SPS_NUT);
    xPutBits(bitstrm, 0, 4);                    // sps_seq_parameter_set_id
    xPutBits(bitstrm, 0, 4);                    // vps_seq_parameter_set_id
    xPutBits(bitstrm, 0, 3);                    // sps_max_sub_layers_minus1
    xPutBits(bitstrm, 1, 2);                    // sps_chroma_format_idc @ 0=400, 1=420, 2=422, 3=444
    xPutBits(bitstrm, 1, 2);                    // sps_log2_ctu_size_minus5 @ [0,2], 3=reserved
    xPutBits(bitstrm, 1, 1);                    // sps_ptl_dpb_hrd_params_present_flag @ must be 1 when SPS_ID_0

    if(1)   // sps_ptl_dpb_hrd_params_present_flag
    {
        codeProfileTierLevel(codec, bitstrm);
    }

    xPutBits(bitstrm, 0, 1);                    // sps_gdr_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_ref_pic_resampling_enabled_flag
    xWriteUvlc(bitstrm, codec->params.nWidth);  // sps_pic_width_max_in_luma_samples
    xWriteUvlc(bitstrm, codec->params.nHeight); // sps_pic_height_max_in_luma_samples
    xPutBits(bitstrm, 0, 1);                    // sps_conformance_window_flag
    xPutBits(bitstrm, 0, 1);                    // sps_subpic_info_present_flag
    xWriteUvlc(bitstrm, 0);                     // sps_bitdepth_minus8
    xPutBits(bitstrm, 1, 1);                    // sps_entropy_coding_sync_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_entry_point_offsets_present_flag
    xPutBits(bitstrm, 4, 4);                    // sps_log2_max_pic_order_cnt_lsb_minus4
    xPutBits(bitstrm, 0, 1);                    // sps_poc_msb_cycle_flag
    xPutBits(bitstrm, 0, 2);                    // sps_num_extra_ph_bytes
    xPutBits(bitstrm, 0, 2);                    // sps_num_extra_sh_bytes
    xWriteUvlc(bitstrm, 1);                     // sps_log2_min_luma_coding_block_size_minus2
    xPutBits(bitstrm, 0, 1);                    // sps_partition_constraints_override_enabled_flag
    xWriteUvlc(bitstrm, 0);                     // sps_log2_diff_min_qt_min_cb_intra_slice_luma
    xWriteUvlc(bitstrm, 4);                     // sps_max_mtt_hierarchy_depth_intra_slice_luma
    if(1)   // sps_max_mtt_hierarchy_depth_intra_slice_luma
    {
        xWriteUvlc(bitstrm, 4);                 // sps_log2_diff_max_bt_min_qt_intra_slice_luma
        xWriteUvlc(bitstrm, 4);                 // sps_log2_diff_max_tt_min_qt_intra_slice_luma
    }
    xPutBits(bitstrm, 0, 1);                    // sps_qtbtt_dual_tree_intra_flag
    if(0)   // sps_qtbtt_dual_tree_intra_flag
    {
    }
    xWriteUvlc(bitstrm, 0);                     // sps_log2_diff_min_qt_min_cb_inter_slice
    xWriteUvlc(bitstrm, 4);                     // sps_max_mtt_hierarchy_depth_inter_slice
    if(1)   // sps_max_mtt_hierarchy_depth_inter_slice
    {
        xWriteUvlc(bitstrm, 4);                 // sps_log2_diff_max_bt_min_qt_inter_slice
        xWriteUvlc(bitstrm, 4);                 // sps_log2_diff_max_tt_min_qt_inter_slice
    }
    xPutBits(bitstrm, 1, 1);                    // sps_max_luma_transform_size_64_flag
    xPutBits(bitstrm, 0, 1);                    // sps_transform_skip_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_mts_enabled_flag
    if(0)   // sps_mts_enabled_flag
    {
    }
    xPutBits(bitstrm, 0, 1);                    // sps_lfnst_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_joint_cbcr_enabled_flag
    xPutBits(bitstrm, 1, 1);                    // sps_same_qp_table_for_chroma_flag
    xWriteSvlc(bitstrm, 0);                     // sps_qp_table_starts_minus26
    xWriteUvlc(bitstrm, 0);                     // sps_num_points_in_qp_table_minus1
    xWriteUvlc(bitstrm, 0);                     // sps_delta_qp_in_val_minus1
    xWriteUvlc(bitstrm, 0);                     // sps_delta_qp_diff_val
    xPutBits(bitstrm, 0, 1);                    // sps_sao_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_alf_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_lmcs_enable_flag
    xPutBits(bitstrm, 0, 1);                    // sps_weighted_pred_flag
    xPutBits(bitstrm, 0, 1);                    // sps_weighted_bipred_flag
    xPutBits(bitstrm, 0, 1);                    // sps_long_term_ref_pics_flag
    xPutBits(bitstrm, 0, 1);                    // sps_idr_rpl_present_flag
    xPutBits(bitstrm, 1, 1);                    // sps_rpl1_same_as_rpl0_flag
    xWriteUvlc(bitstrm, 0);                     // sps_num_ref_pic_lists[0]
    xPutBits(bitstrm, 0, 1);                    // sps_ref_wraparound_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_temporal_mvp_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_amvr_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_bdof_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_smvd_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_dmvr_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_mmvd_enabled_flag
    xWriteUvlc(bitstrm, 0);                     // sps_six_minus_max_num_merge_cand
    xPutBits(bitstrm, 0, 1);                    // sps_sbt_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_affine_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_bcw_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_ciip_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_gpm_enabled_flag
    xWriteUvlc(bitstrm, 0);                     // sps_log2_parallel_merge_level_minus2
    xPutBits(bitstrm, 0, 1);                    // sps_isp_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_mrl_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_mip_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_cclm_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_chroma_horizontal_collocated_flag
    xPutBits(bitstrm, 0, 1);                    // sps_chroma_vertical_collocated_flag
    xPutBits(bitstrm, 0, 1);                    // sps_palette_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_ibc_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_ladf_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_scaling_list_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_dep_quant_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_sign_data_hiding_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_virtual_boundaries_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // sps_field_seq_flag
    xPutBits(bitstrm, 0, 1);                    // sps_vui_parameters_present_flag
    xPutBits(bitstrm, 0, 1);                    // sps_extension_present_flag
    xWriteAlignOne(bitstrm);
    xBitFlush(bitstrm);
}

void xWritePPS(codec_t *codec)
{
    bitStream_t *bitstrm = &codec->bitstrm;

    xWriteNALHeader(bitstrm, PPS_NUT);
    xPutBits(bitstrm, 0, 6);                    // pps_pic_parameter_set_id
    xPutBits(bitstrm, 0, 4);                    // pps_seq_parameter_set_id
    xPutBits(bitstrm, 0, 1);                    // pps_mixed_nalu_types_in_pic_flag
    xWriteUvlc(bitstrm, codec->params.nWidth);  // pps_pic_width_in_luma_samples
    xWriteUvlc(bitstrm, codec->params.nHeight); // pps_pic_height_in_luma_samples
    xPutBits(bitstrm, 0, 1);                    // pps_conformance_window_flag
    xPutBits(bitstrm, 0, 1);                    // pps_scaling_window_explicit_signalling_flag
    xPutBits(bitstrm, 1, 1);                    // pps_output_flag_present_flag
    xPutBits(bitstrm, 1, 1);                    // pps_no_pic_partition_flag
    xPutBits(bitstrm, 0, 1);                    // pps_subpic_id_mapping_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_cabac_init_present_flag
    xWriteUvlc(bitstrm, 0);                     // pps_num_ref_idx_default_active_minus1[0]
    xWriteUvlc(bitstrm, 0);                     // pps_num_ref_idx_default_active_minus1[1]
    xPutBits(bitstrm, 0, 1);                    // pps_rpl1_idx_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_weighted_pred_flag
    xPutBits(bitstrm, 0, 1);                    // pps_weighted_bipred_flag
    xPutBits(bitstrm, 0, 1);                    // pps_ref_wraparound_enabled_flag
    xWriteSvlc(bitstrm, 0);                     // pps_init_qp_minus26
    xPutBits(bitstrm, 0, 1);                    // pps_cu_qp_delta_enabled_flag
    xPutBits(bitstrm, 0, 1);                    // pps_chroma_tool_offsets_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_deblocking_filter_control_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_picture_header_extension_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_slice_header_extension_present_flag
    xPutBits(bitstrm, 0, 1);                    // pps_extension_flag
    xWriteAlignOne(bitstrm);
    xBitFlush(bitstrm);
}

void xWritePictureHeader(codec_t *codec, const int writeRbspTrailingBits)
{
    bitStream_t *bitstrm = &codec->bitstrm;

    xWriteNALHeader(bitstrm, PH_NUT);
    xPutBits(bitstrm, 0, 1);                    // ph_gdr_or_irap_pic_flag
    xPutBits(bitstrm, 0, 1);                    // ph_non_ref_pic_flag
    xPutBits(bitstrm, 0, 1);                    // ph_inter_slice_allowed_flag
    xWriteUvlc(bitstrm, 0);                     // ph_pic_parameter_set_id
    xPutBits(bitstrm, 0, 8);                    // ph_pic_order_cnt_lsb
    xPutBits(bitstrm, 0, 1);                    // ph_pic_output_flag

    if(writeRbspTrailingBits)
        xWriteAlignOne(bitstrm);
    xBitFlush(bitstrm);
}

void xWriteSliceHeader(codec_t *codec)
{
    bitStream_t *bitstrm = &codec->bitstrm;

    xWriteNALHeader(bitstrm, RADL_NUT);
    xPutBits(bitstrm, 0, 1);                    // sh_picture_header_in_slice_header_flag

    xWriteAlignOne(bitstrm);
    xBitFlush(bitstrm);
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

int xCodecInit(codec_t *codec, param_t *params)
{
    int i;
    const uint32_t nWidth = params->nWidth;
    const uint32_t nHeight = params->nHeight;

    memset(codec, 0, sizeof(codec_t));

    codec->params = *params;
    codec->m_frames_strd = nWidth / REF_BLOCK_SZ;

    for(i = 0; i < ASIZE(codec->m_frames); i++)
    {
        codec->m_frames[i] = (ref_block_t *)_aligned_malloc(nWidth * nHeight / (REF_BLOCK_SZ * REF_BLOCK_SZ) * sizeof(ref_block_t), 4096);

        if(codec->m_frames[i] == NULL)
            return -1;
    }
    return 0;
}

void xCodecFree(codec_t *codec)
{
    int i;

    for(i = 0; i < ASIZE(codec->m_frames); i++)
    {
        if(codec->m_frames[i])
            _aligned_free((codec->m_frames[i]));
    }
}

int xEncodeFrame(codec_t *codec,
                 uint8_t *buf,
                 const uint8_t *srcY,
                 const uint8_t *srcU,
                 const uint8_t *srcV,
                 const intptr_t strdY)
{
    const uint32_t nWidth = codec->params.nWidth;
    const uint32_t nHeight = codec->params.nHeight;

    // Convert format to internal
    xConvInputFmt(codec->m_frames[0],
                  srcY,
                  srcU,
                  srcV,
                  strdY,
                  nWidth,
                  nHeight);

    // Encode
    xBitStreamInit(&codec->bitstrm, buf, BIT_BUF_SIZE);

    // Headers
    xWriteSPS(codec);
    xWritePPS(codec);
    xWritePictureHeader(codec, 1);
    xWriteSliceHeader(codec);

    return xBitFlush(&codec->bitstrm);
}

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
    uint32_t nWidth = 0;
    uint32_t nHeight = 0;
    int nFrames = 0;
    uint8_t *frameBuf = NULL;
    uint8_t *bitBuf = NULL;

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

    int ret = 0;

    // Validate parameters
    if(nWidth == 0 || nWidth > MAX_WIDTH ||
       nHeight == 0 || nHeight > MAX_HEIGHT ||
       fpi == NULL || fpo == NULL)
    {
        fprintf(stderr, "Parameters check failed\n");
        ret = -1;
        goto _cleanup;
    }

    // Unit Tests
#if TEST_xConvInputFmt
    {
        int i, j;
        uint8_t tmp[32*16+2*32*16/4];
        memset(tmp, 0xCC, sizeof(tmp));

        for(i = 0; i < 32*16+2*32*16/4; i++)
        {
            tmp[i] = i;
        }

        ref_block_t blocks[2];
        memset(blocks, 0xCD, sizeof(blocks));

        xConvInputFmt(blocks, &tmp[0], &tmp[256], &tmp[320], 32, 32, 16);
        printf("xConvFmt Done\n");

        // for test only
        memset(frameBuf, 0xCD, nWidth * nHeight * 3 / 2 * sizeof(uint8_t));
        xConvOutput420(codec.m_frames[0],
                       frameBuf,
                       nWidth,
                       frameBuf + nWidth * nHeight,
                       frameBuf + nWidth * nHeight * 5 / 4,
                       nWidth / 2,
                       nWidth,
                       nHeight);

    }
#endif

    // Initialize codec
    const uint32_t frameSize = nWidth * nHeight * 3 / 2;
    frameBuf = (uint8_t*)_aligned_malloc(frameSize * sizeof(uint8_t), 4096);
    assert(frameBuf && "frameBuf Memory allocate failed\n");
    bitBuf = (uint8_t*)_aligned_malloc(BIT_BUF_SIZE * sizeof(uint8_t), 64);
    assert(bitBuf && "bitBuf Memory allocate failed\n");

    codec_t codec;
    param_t params;

    params.nWidth = nWidth;
    params.nHeight = nHeight;

    if(xCodecInit(&codec, &params))
    {
        fprintf(stderr, "Codec init failed\n");
        ret = -2;
        goto _cleanup;
    }

    // Encode loop
    for(i = 0; i < nFrames; i++)
    {
        // Prepare input frame
        if(frameSize != fread(frameBuf, 1, frameSize, fpi))
        {
            fprintf(stderr, "Read file failed\n");
            goto _cleanup;
        }

        // Encode
        int nSize = xEncodeFrame(&codec,
                                 bitBuf,
                                 frameBuf,
                                 frameBuf + nWidth * nHeight,
                                 frameBuf + nWidth * nHeight * 5 / 4,
                                 nWidth);

        fwrite(bitBuf, 1, nSize, fpo);
    }
    printf("Encode %d frames done\n", i);

    // Cleanup
_cleanup:
    if(fpi)
        fclose(fpi);
    if(fpo)
        fclose(fpo);
    if(frameBuf)
        _aligned_free(frameBuf);
    if(bitBuf)
        _aligned_free(bitBuf);

    xCodecFree(&codec);

    return ret;
}

