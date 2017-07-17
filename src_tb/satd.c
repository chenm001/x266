// Copyright (c) 2016-2017 Min Chen
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#include <stdio.h>
#include <stdlib.h>
#include "tb_common.h"

static int satd8x8(const int16_t diff[64])
{
    int i, ii;
    int32_t sad = 0;
    int16_t m1[8][8], m2[8][8], m3[8][8];

    //horizontal
    for (i=0; i < 8; i++)
    {
      ii = i << 3;
      m2[i][0] = diff[ii  ] + diff[ii+4];
      m2[i][1] = diff[ii+1] + diff[ii+5];
      m2[i][2] = diff[ii+2] + diff[ii+6];
      m2[i][3] = diff[ii+3] + diff[ii+7];
      m2[i][4] = diff[ii  ] - diff[ii+4];
      m2[i][5] = diff[ii+1] - diff[ii+5];
      m2[i][6] = diff[ii+2] - diff[ii+6];
      m2[i][7] = diff[ii+3] - diff[ii+7];

      m1[i][0] = m2[i][0] + m2[i][2];
      m1[i][1] = m2[i][1] + m2[i][3];
      m1[i][2] = m2[i][0] - m2[i][2];
      m1[i][3] = m2[i][1] - m2[i][3];
      m1[i][4] = m2[i][4] + m2[i][6];
      m1[i][5] = m2[i][5] + m2[i][7];
      m1[i][6] = m2[i][4] - m2[i][6];
      m1[i][7] = m2[i][5] - m2[i][7];

      m2[i][0] = m1[i][0] + m1[i][1];
      m2[i][1] = m1[i][0] - m1[i][1];
      m2[i][2] = m1[i][2] + m1[i][3];
      m2[i][3] = m1[i][2] - m1[i][3];
      m2[i][4] = m1[i][4] + m1[i][5];
      m2[i][5] = m1[i][4] - m1[i][5];
      m2[i][6] = m1[i][6] + m1[i][7];
      m2[i][7] = m1[i][6] - m1[i][7];
    }

    //vertical
    for (i=0; i < 8; i++)
    {
      m3[0][i] = m2[0][i] + m2[4][i];
      m3[1][i] = m2[1][i] + m2[5][i];
      m3[2][i] = m2[2][i] + m2[6][i];
      m3[3][i] = m2[3][i] + m2[7][i];
      m3[4][i] = m2[0][i] - m2[4][i];
      m3[5][i] = m2[1][i] - m2[5][i];
      m3[6][i] = m2[2][i] - m2[6][i];
      m3[7][i] = m2[3][i] - m2[7][i];

      m1[0][i] = m3[0][i] + m3[2][i];
      m1[1][i] = m3[1][i] + m3[3][i];
      m1[2][i] = m3[0][i] - m3[2][i];
      m1[3][i] = m3[1][i] - m3[3][i];
      m1[4][i] = m3[4][i] + m3[6][i];
      m1[5][i] = m3[5][i] + m3[7][i];
      m1[6][i] = m3[4][i] - m3[6][i];
      m1[7][i] = m3[5][i] - m3[7][i];

      m2[0][i] = m1[0][i] + m1[1][i];
      m2[1][i] = m1[0][i] - m1[1][i];
      m2[2][i] = m1[2][i] + m1[3][i];
      m2[3][i] = m1[2][i] - m1[3][i];
      m2[4][i] = m1[4][i] + m1[5][i];
      m2[5][i] = m1[4][i] - m1[5][i];
      m2[6][i] = m1[6][i] + m1[7][i];
      m2[7][i] = m1[6][i] - m1[7][i];
    }

    for (i = 0; i < 8; i++)
    {
        for (j = 0; j < 8; j++)
        {
            sad += abs(m2[i][j]);
        }
    }

    sad=((sad+2)>>2);

    return sad;
}

static int16_t mat[8 * 8];
static int lastRow = 0;
static int lastSatd = 0;

void satd8x8_genNew()
{
    int i, j;

    for(i = 0; i < 8; i++)
    {
        for(j = 0; j < 8; j++)
        {
            const int a = rand() & 0xFF;
            const int b = rand() & 0xFF;
            mat[i * 8 + j] = (a - b);
        }
    }

    lastSatd  = satd8x8(mat);
    lastRow = 0;
}

// Vector#(8, Bit#(9)))
void satd8x8_getDiff(unsigned int res[])
{
    memcpy(res, &mat[lastRow * 8], 8 * sizeof(*mat));
    lastRow++;
}

uint32_t satd8x8_getSatd(/*unsigned int res[]*/)
{
    return lastSatd;
}
