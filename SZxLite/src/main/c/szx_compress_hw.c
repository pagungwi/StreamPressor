#include "define.h"
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "szx.h"
#include <time.h>
#include "rocc.h"

// Prev malloc() were arbitrarily aligned buffers - ROCC L1D cache is 64B
// Defines lenght of cache line and matches SZxROCCConfig.scala
#define SZX_CACHE_LINE_BYTES 64
// Hardware acceleration control
int g_use_hardware_acceleration = 1; // Global flag controlled from main (0 = use software, 1 = use hardware)

// Helper function to convert float to uint32_t (bitwise)
uint32_t float_to_bits(float f) {
    union {
        float f;
        uint32_t i;
    } u;
    u.f = f;
    return u.i;
}

inline void longToBytes_bigEndian(unsigned char *b, unsigned long num)
{
        b[0] = (unsigned char)(num>>56);
        b[1] = (unsigned char)(num>>48);
        b[2] = (unsigned char)(num>>40);
        b[3] = (unsigned char)(num>>32);
        b[4] = (unsigned char)(num>>24);
        b[5] = (unsigned char)(num>>16);
        b[6] = (unsigned char)(num>>8);
        b[7] = (unsigned char)(num);
}

inline void sizeToBytes(unsigned char* outBytes, size_t size)
{
                longToBytes_bigEndian(outBytes, size);//8
}

inline short getExponent_float(float value)
{
        //int ivalue = floatToBigEndianInt(value);

        lfloat lbuf;
        lbuf.value = value;
        int ivalue = lbuf.ivalue;

        int expValue = (ivalue & 0x7F800000) >> 23;
        expValue -= 127;
        return (short)expValue;
}

inline void floatToBytes(unsigned char *b, float num)
{
        lfloat buf;
        buf.value = num;
        memcpy(b, buf.byte, 4);
}

/*Compute the number of significant based on the user-required error bound (realPrecision)*/
inline void computeReqLength_float(double realPrecision, short radExpo, int *reqLength, float *medianValue) {
    short reqExpo = getPrecisionReqLength_double(realPrecision);
    *reqLength = 9 + radExpo - reqExpo + 1; //radExpo-reqExpo == reqMantiLength
    if (*reqLength < 9)
        *reqLength = 9;
    if (*reqLength > 32)
    {
        *reqLength = 32;
        *medianValue = 0;
    }
}

inline short getPrecisionReqLength_double(double precision)
{
        ldouble lbuf;
        lbuf.value = precision;
        long lvalue = lbuf.lvalue;

        int expValue = (int)((lvalue & 0x7FF0000000000000) >> 52);
        expValue -= 1023;

        return (short)expValue;
}

/*compute value range and mean for each block of the whole dataset*/
size_t computeStateMedianRadius_float(float *oriData, size_t nbEle, float absErrBound, int blockSize,
                                      unsigned char *stateArray, float *medianArray, float *radiusArray) {
    size_t nbConstantBlocks = 0;
    size_t i = 0, j = 0;
    size_t nbBlocks = nbEle / blockSize;
    size_t offset = 0;

    for (i = 0; i < nbBlocks; i++) {
        float min = oriData[offset];
        float max = oriData[offset];
        for (j = 1; j < (size_t)blockSize; j++) {
            float v = oriData[offset + j];
            if (min > v)
                min = v;
            else if (max < v)
                max = v;
        }
        float valueRange = max - min;
        float radius = valueRange / 2;
        float medianValue = min + radius;

        if (radius <= absErrBound) {
            stateArray[i] = 0;
            nbConstantBlocks++;
        } else
            stateArray[i] = 1;

        stateArray[i] = radius <= absErrBound ? 0 : 1;
        medianArray[i] = medianValue;
        radiusArray[i] = radius;
        offset += blockSize;
    }

    int remainCount = nbEle % blockSize;
    if (remainCount != 0) {
        float min = oriData[offset];
        float max = oriData[offset];
        for (j = 1; j < (size_t)remainCount; j++) {
            float v = oriData[offset + j];
            if (min > v)
                min = v;
            else if (max < v)
                max = v;
        }
        float valueRange = max - min;
        float radius = valueRange / 2;
        float medianValue = min + radius;
        if (radius <= absErrBound) {
            stateArray[i] = 0;
            nbConstantBlocks++;
        } else
            stateArray[i] = 1;
        medianArray[i] = medianValue;
        radiusArray[i] = radius;
    }
    return nbConstantBlocks;
}

// Forward declaration
void SZx_compress_one_block_float_sw(float *oriData, size_t nbEle, float absErrBound,
                                    unsigned char *outputBytes, int *outSize,
                                    unsigned char *leadNumberArray_int, float medianValue,
                                    float radius);

/* OLD Hardware-accelerated block compression function
void SZx_compress_one_block_float_hw(float *oriData, size_t nbEle, float absErrBound,
                                           unsigned char *outputBytes, int *outSize,
                                           unsigned char *leadNumberArray_int, float medianValue,
                                           float radius) {

    printf("HW: Starting hardware compression\n");

    // Configure the accelerator
    szx_config(absErrBound, medianValue);
    szx_set_radius(radius);

    // Load data into RoCC accelerator using bulk loading
    szx_load_block_bulk((uint32_t)oriData, (uint32_t)nbEle);

    // Perform compression using RoCC
    uint32_t compressed_size = szx_compress(oriData, outputBytes);

    printf("HW: RoCC compression complete, size: %u\n", compressed_size);
    *outSize = compressed_size;

    // The RoCC accelerator should have written the compressed data to outputBytes
    // The size should be the actual compressed size from the hardware
} */
/* NEW Hardware-accelerated block compression function
    - SZx_compress_float responsible for passing cache-aligned pointer
    - szx_load_block_bulk() given aligned pointer and explicit word count
    - szx_compress() given only output buffer (input address no longer needed after bulk load)
    - Add assertion to catch misaligned pointers at runtime
*/
void SZx_compress_one_block_float_hw(float *oriData, size_t nbEle, float absErrBound,
                                           unsigned char *outputBytes, int *outSize,
                                           unsigned char *leadNumberArray_int, float medianValue,
                                           float radius) {
    // Runtime alignment check - ensure use posix_memalign
    if ((uintptr_t)oriData % SZX_CACHE_LINE_BYTES != 0) {
        fprintf(stderr, "SZx HW WARNING: oriData pointer %p is not %d-byte aligned. "
                "Cache line straddling will occur.\n", (void*)oriData, SZX_CACHE_LINE_BYTES);
    }
    printf("HW: Starting hardware compression\n");
 
    // Configure the accelerator with error bound, median, and radius
    szx_config(absErrBound, medianValue);
    szx_set_radius(radius);
 
    /* Bulk load using the hardware's io.mem path (funct=6).
        - HW sBulkLoad state issues requests to fetch nbEle 32-bit words from address of oriData
        - Use bulkTransferAddr + (bulkTransferIndex << 2) to step through each word
        - Return a response once all nbEle words have been fetched and stored in scratchpad

        Adds resilience for 64b build to map to 32b RoCC register
    */
    szx_load_block_bulk((uint32_t)(uintptr_t)oriData, (uint32_t)nbEle);
 
    /* Compress data from HW scratchpad instead of CPU-side address
        - Maps to COMPRESS_BLOCK and GET_RESULT in HW state machine
    */
    uint32_t compressed_size = szx_compress_from_scratchpad(outputBytes);
 
    printf("HW: RoCC compression complete, size: %u\n", compressed_size);
    *outSize = compressed_size;
}

// Original software block compression function
inline void SZx_compress_one_block_float_sw(float *oriData, size_t nbEle, float absErrBound,
                                           unsigned char *outputBytes, int *outSize,
                                           unsigned char *leadNumberArray_int, float medianValue,
                                           float radius) {
    size_t totalSize = 0, i = 0;

    int reqLength;

    //compute median, value range, and radius

    short radExpo = getExponent_float(radius);
    computeReqLength_float(absErrBound, radExpo, &reqLength, &medianValue);

    int reqBytesLength = reqLength / 8;
    int resiBitsLength = reqLength % 8;
    int rightShiftBits = 0;

    size_t leadNumberArray_size = nbEle % 4 == 0 ? nbEle / 4 : nbEle / 4 + 1;

    register lfloat lfBuf_pre;
    register lfloat lfBuf_cur;
    lfBuf_pre.ivalue = 0;

    unsigned char *leadNumberArray = outputBytes + 1 + sizeof(float);

    unsigned char *exactMidbyteArray = leadNumberArray + leadNumberArray_size;

    if (resiBitsLength != 0) {
        rightShiftBits = 8 - resiBitsLength;
        reqBytesLength++;
    }

    register unsigned char leadingNum = 0;
    size_t residualMidBytes_size = 0;

    if (reqBytesLength == 2) {
        for (i = 0; i < nbEle; i++) {
            leadingNum = 0;
            lfBuf_cur.value = oriData[i] - medianValue;

            lfBuf_cur.ivalue = lfBuf_cur.ivalue >> rightShiftBits;

            lfBuf_pre.ivalue = lfBuf_cur.ivalue ^ lfBuf_pre.ivalue;

            if (lfBuf_pre.ivalue >> 8 == 0)
                leadingNum = 3;
            else if (lfBuf_pre.ivalue >> 16 == 0)
                leadingNum = 2;
            else if (lfBuf_pre.ivalue >> 24 == 0)
                leadingNum = 1;

            leadNumberArray_int[i] = leadingNum;

            if (leadingNum == 0) {
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[2];
                exactMidbyteArray[residualMidBytes_size + 1] = lfBuf_cur.byte[3];
                residualMidBytes_size += 2;
            } else if (leadingNum == 1) {
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[2];
                residualMidBytes_size++;
            }

            lfBuf_pre = lfBuf_cur;
        }
    } else if (reqBytesLength == 3) {
        for (i = 0; i < nbEle; i++) {
            leadingNum = 0;
            lfBuf_cur.value = oriData[i] - medianValue;

            lfBuf_cur.ivalue = lfBuf_cur.ivalue >> rightShiftBits;

            lfBuf_pre.ivalue = lfBuf_cur.ivalue ^ lfBuf_pre.ivalue;

            if (lfBuf_pre.ivalue >> 8 == 0)
                leadingNum = 3;
            else if (lfBuf_pre.ivalue >> 16 == 0)
                leadingNum = 2;
            else if (lfBuf_pre.ivalue >> 24 == 0)
                leadingNum = 1;

            leadNumberArray_int[i] = leadingNum;

            if (leadingNum == 0) {
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[2];
                exactMidbyteArray[residualMidBytes_size + 1] = lfBuf_cur.byte[3];
                exactMidbyteArray[residualMidBytes_size + 2] = lfBuf_cur.byte[0];
                residualMidBytes_size += 3;
            } else if (leadingNum == 1) {
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[2];
                exactMidbyteArray[residualMidBytes_size + 1] = lfBuf_cur.byte[3];
                residualMidBytes_size += 2;
            } else if (leadingNum == 2) {
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[2];
                residualMidBytes_size++;
            } else { // leadingNum == 3
                exactMidbyteArray[residualMidBytes_size] = lfBuf_cur.byte[0];
                exactMidbyteArray[residualMidBytes_size + 1] = lfBuf_cur.byte[1];
                residualMidBytes_size += 2;
            }

            lfBuf_pre = lfBuf_cur;
        }
    }

    convertIntArray2ByteArray_fast_2b_args(leadNumberArray_int, nbEle, leadNumberArray);
    int k = 0;

    unsigned char reqLengthB = (unsigned char) reqLength;
    outputBytes[k] = reqLengthB;
    k++;
    floatToBytes(&(outputBytes[k]), medianValue);
    k += sizeof(float);

    totalSize = 1 + sizeof(float) + leadNumberArray_size + residualMidBytes_size;

    int s_actual_leadNumbers = 0;
    for(size_t j = 0;j<nbEle;j++)
    {
        if(leadNumberArray_int[j] >= reqBytesLength)
            s_actual_leadNumbers += reqBytesLength;
        else
            s_actual_leadNumbers += leadNumberArray_int[j];
    }

    *outSize = totalSize;
}

// Main block compression function that chooses between hardware and software
inline void SZx_compress_one_block_float(float *oriData, size_t nbEle, float absErrBound,
                                        unsigned char *outputBytes, int *outSize,
                                        unsigned char *leadNumberArray_int, float medianValue,
                                        float radius) {

    if (g_use_hardware_acceleration) {
        // Use hardware acceleration
        SZx_compress_one_block_float_hw(oriData, nbEle, absErrBound, outputBytes, outSize,
                                       leadNumberArray_int, medianValue, radius);
    } else {
        // Use software implementation
        SZx_compress_one_block_float_sw(oriData, nbEle, absErrBound, outputBytes, outSize,
                                       leadNumberArray_int, medianValue, radius);
    }
}

/*convert 1bit_map int_array to byte array*/
size_t convertIntArray2ByteArray_fast_1b_args(unsigned char* intArray, size_t intArrayLength, unsigned char *result)
{
        size_t byteLength = 0;
        size_t i, j;
        if(intArrayLength%8==0)
                byteLength = intArrayLength/8;
        else
                byteLength = intArrayLength/8+1;

        size_t n = 0;
        int tmp, type;
        for(i = 0;i<byteLength;i++)
        {
                tmp = 0;
                for(j = 0;j<8&&n<intArrayLength;j++)
                {
                        type = intArray[n];
                        //if(type == 1)
                        tmp = (tmp | (type << (7-j)));
                        n++;
                }
        result[i] = (unsigned char)tmp;
        }
        return byteLength;
}

/*convert xor_leading_state_int_array to byte array*/
inline size_t convertIntArray2ByteArray_fast_2b_args(unsigned char* timeStepType, size_t timeStepTypeLength, unsigned char *result)
{
        register unsigned char tmp = 0;
        size_t i, j = 0, byteLength = 0;
        if(timeStepTypeLength%4==0)
                byteLength = timeStepTypeLength*2/8;
        else
                byteLength = timeStepTypeLength*2/8+1;
        size_t n = 0;
        if(timeStepTypeLength%4==0)
        {
                for(i = 0;i<byteLength;i++)
                {
                        tmp = 0;

                        tmp |= timeStepType[n++] << 6;
                        tmp |= timeStepType[n++] << 4;
                        tmp |= timeStepType[n++] << 2;
                        tmp |= timeStepType[n++];

                        result[i] = tmp;
                }
        }
        else
        {
                size_t byteLength_ = byteLength - 1;
                for(i = 0;i<byteLength_;i++)
                {
                        tmp = 0;

                        tmp |= timeStepType[n++] << 6;
                        tmp |= timeStepType[n++] << 4;
                        tmp |= timeStepType[n++] << 2;
                        tmp |= timeStepType[n++];

                        result[i] = tmp;
                }
                tmp = 0;
        int mod4 = timeStepTypeLength%4;
        for(size_t j=0;j<(size_t)mod4;j++)
                {
                        unsigned char type = timeStepType[n++];
                        tmp = tmp | type << (6-(j<<1));
                }
                result[i] = tmp;
        }

        return byteLength;
}

unsigned char *
SZx_compress_float(float *oriData, size_t *outSize, float absErrBound,
    size_t nbEle, int blockSize) {

    // oriData alignment check that prints warning for test application
    if ((uintptr_t)oriData % SZX_CACHE_LINE_BYTES != 0) {
        fprintf(stderr, "SZx WARNING: oriData %p is not %d-byte aligned. "
                "Hardware bulk load may straddle cache line boundaries. "
                "Use posix_memalign in the caller.\n",
                (void*)oriData, SZX_CACHE_LINE_BYTES);
    }
    
    float *op = oriData;

    *outSize = 0;
    size_t maxPreservedBufferSize =
            sizeof(float) * nbEle; //assume that the compressed data size would not exceed the original size
    
    // Aligned output buffer allocation: changing mallocs to cache-aligned wit posix_memalign()
    size_t alignedSize = ((maxPreservedBufferSize + SZX_CACHE_LINE_BYTES - 1) / SZX_CACHE_LINE_BYTES) * SZX_CACHE_LINE_BYTES;
    unsigned char *outputBytes = NULL;
    if (posix_memalign((void**)&outputBytes, SZX_CACHE_LINE_BYTES, alignedSize) != 0) {
        fprintf(stderr, "SZx ERROR: posix_memalign failed for outputBytes\n");
        return NULL;
    }

    unsigned char *leadNumberArray_int = (unsigned char *) malloc(blockSize * sizeof(int));

    size_t i = 0;
    int oSize = 0;

    size_t nbBlocks = nbEle / blockSize;
    size_t remainCount = nbEle % blockSize;
    size_t actualNBBlocks = remainCount == 0 ? nbBlocks : nbBlocks + 1;

    size_t stateNBBytes = (actualNBBlocks % 8 == 0 ? actualNBBlocks / 8 : actualNBBlocks / 8 + 1);

    unsigned char *stateArray = (unsigned char *) malloc(actualNBBlocks);
    float *medianArray = (float *) malloc(actualNBBlocks * sizeof(float));
    float *radiusArray = (float *) malloc(actualNBBlocks * sizeof(float));

    size_t nbConstantBlocks = computeStateMedianRadius_float(oriData, nbEle, absErrBound, blockSize, stateArray,
                                                             medianArray, radiusArray);

    size_t nbNonConstantBlocks = actualNBBlocks - nbConstantBlocks;

    unsigned char *r = outputBytes; // + sizeof(size_t) + stateNBBytes;
    r[0] = SZx_VER_MAJOR;
    r[1] = SZx_VER_MINOR;
    r[2] = 1;
    r[3] = 1; //support random access decompression
    r = r + 4; //1 byte

    sizeToBytes(r, blockSize);
    r += sizeof(size_t);
    sizeToBytes(r, nbConstantBlocks);
    r += sizeof(size_t); //r is the starting address of 'block-size array'
    uint16_t *O=(uint16_t*)r;
    unsigned char *R = r + nbNonConstantBlocks*sizeof(uint16_t); //R is the starting address of the state array
    unsigned char *p = R + stateNBBytes; //p is the starting address of constant median values.
    unsigned char *q =
            p + sizeof(float) * nbConstantBlocks; //q is the starting address of the non-constant data sblocks
    //3: versions, 1: metadata: state, 1: metadata: blockSize, sizeof(size_t): nbConstantBlocks, ....
    *outSize = q-outputBytes;

    size_t nonConstantBlockID = 0;
    printf("nbConstantBlocks = %lu, percent = %f\n", (unsigned long)nbConstantBlocks, 1.0f*(nbConstantBlocks*blockSize)/nbEle);
    printf("Processing %lu blocks, %lu non-constant blocks\n", nbBlocks, nbNonConstantBlocks);
    fflush(stdout);

    /*
        Advance by blockSize # of floats each iteration
        Prefetch all cache lines needed to get all floats for that next block to be processed
    */
    for (i = 0; i < nbBlocks; i++, op += blockSize) {
        // Only print for first 5 blocks and then every 10th block
        if (i < 5 || i % 10 == 0) {
            printf("Processing block %lu (state: %d)\n", i, stateArray[i]);
            fflush(stdout);
        }

        if (stateArray[i]) {
            if (i < 5 || i % 10 == 0) {
                printf("Block %lu: Using software compression\n", i);
                fflush(stdout);
            }
            SZx_compress_one_block_float(op, blockSize, absErrBound, q, &oSize,
                                       leadNumberArray_int, medianArray[i], radiusArray[i]);
            if (i < 5 || i % 10 == 0) {
                printf("Block %lu: Software compression complete, size: %d\n", i, oSize);
                fflush(stdout);
            }
            q += oSize;
            *outSize += oSize;
            O[nonConstantBlockID++] = oSize;
        } else {
            if (i < 5 || i % 10 == 0) {
                printf("Block %lu: Using constant block (no compression needed)\n", i);
                fflush(stdout);
            }
            floatToBytes(p, medianArray[i]);
            p += sizeof(float);
        }
    }

    if (remainCount != 0) {
        if (stateArray[i]) {
            SZx_compress_one_block_float(op, remainCount, absErrBound, q, &oSize,
                                       leadNumberArray_int, medianArray[i], radiusArray[i]);
            *outSize += oSize;
            O[nonConstantBlockID] = oSize;
        } else {
            floatToBytes(p, medianArray[i]);
        }

    }

    convertIntArray2ByteArray_fast_1b_args(stateArray, actualNBBlocks, R);

    free(leadNumberArray_int);

    return outputBytes;
}
