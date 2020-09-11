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

