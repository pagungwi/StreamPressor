// Based on code by Schuyler Eldridge. Copyright (c) Boston University
// https://github.com/seldridge/rocket-rocc-examples/blob/master/src/main/c/rocc.h

#ifndef SRC_MAIN_C_ROCC_H
#define SRC_MAIN_C_ROCC_H

#include <stdint.h>

#define STR1(x) #x
#define STR(x) STR1(x)
#define EXTRACT(a, size, offset) (((~(~0 << size) << offset) & a) >> offset)

#define CUSTOMX_OPCODE(x) CUSTOM_ ## x
#define CUSTOM_0 0b0001011
#define CUSTOM_1 0b0101011
#define CUSTOM_2 0b1011011
#define CUSTOM_3 0b1111011

#define CUSTOMX(X, xd, xs1, xs2, rd, rs1, rs2, funct) \
  CUSTOMX_OPCODE(X)                     |             \
  (rd                 << (7))           |             \
  (xs2                << (7+5))         |             \
  (xs1                << (7+5+1))       |             \
  (xd                 << (7+5+2))       |             \
  (rs1                << (7+5+3))       |             \
  (rs2                << (7+5+3+5))     |             \
  (EXTRACT(funct, 7, 0) << (7+5+3+5+5))

// Standard macro that passes rd, rs1, and rs2 via registers
#define ROCC_INSTRUCTION_DSS(X, rd, rs1, rs2, funct) \
        ROCC_INSTRUCTION_R_R_R(X, rd, rs1, rs2, funct, 10, 11, 12)

#define ROCC_INSTRUCTION_DS(X, rd, rs1, funct) \
        ROCC_INSTRUCTION_R_R_I(X, rd, rs1, 0, funct, 10, 11)

#define ROCC_INSTRUCTION_D(X, rd, funct) \
        ROCC_INSTRUCTION_R_I_I(X, rd, 0, 0, funct, 10)

#define ROCC_INSTRUCTION_SS(X, rs1, rs2, funct) \
        ROCC_INSTRUCTION_I_R_R(X, 0, rs1, rs2, funct, 11, 12)

#define ROCC_INSTRUCTION_S(X, rs1, funct) \
        ROCC_INSTRUCTION_I_R_I(X, 0, rs1, 0, funct, 11)

#define ROCC_INSTRUCTION(X, funct) \
        ROCC_INSTRUCTION_I_I_I(X, 0, 0, 0, funct)

// rd, rs1, and rs2 are data
// rd_n, rs_1, and rs2_n are the register numbers to use
#define ROCC_INSTRUCTION_R_R_R(X, rd, rs1, rs2, funct, rd_n, rs1_n, rs2_n) { \
    register uint64_t rd_  asm ("x" # rd_n);                                 \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;               \
    register uint64_t rs2_ asm ("x" # rs2_n) = (uint64_t) rs2;               \
    asm volatile (                                                           \
        ".word " STR(CUSTOMX(X, 1, 1, 1, rd_n, rs1_n, rs2_n, funct)) "\n\t"  \
        : "=r" (rd_)                                                         \
        : [_rs1] "r" (rs1_), [_rs2] "r" (rs2_));                             \
    rd = rd_;                                                                \
  }

#define ROCC_INSTRUCTION_R_R_I(X, rd, rs1, rs2, funct, rd_n, rs1_n) {     \
    register uint64_t rd_  asm ("x" # rd_n);                              \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;            \
    asm volatile (                                                        \
        ".word " STR(CUSTOMX(X, 1, 1, 0, rd_n, rs1_n, rs2, funct)) "\n\t" \
        : "=r" (rd_) : [_rs1] "r" (rs1_));                                \
    rd = rd_;                                                             \
  }

#define ROCC_INSTRUCTION_R_I_I(X, rd, rs1, rs2, funct, rd_n) {           \
    register uint64_t rd_  asm ("x" # rd_n);                             \
    asm volatile (                                                       \
        ".word " STR(CUSTOMX(X, 1, 0, 0, rd_n, rs1, rs2, funct)) "\n\t"  \
        : "=r" (rd_));                                                   \
    rd = rd_;                                                            \
  }

#define ROCC_INSTRUCTION_I_R_R(X, rd, rs1, rs2, funct, rs1_n, rs2_n) {    \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;            \
    register uint64_t rs2_ asm ("x" # rs2_n) = (uint64_t) rs2;            \
    asm volatile (                                                        \
        ".word " STR(CUSTOMX(X, 0, 1, 1, rd, rs1_n, rs2_n, funct)) "\n\t" \
        :: [_rs1] "r" (rs1_), [_rs2] "r" (rs2_));                         \
  }

#define ROCC_INSTRUCTION_I_R_I(X, rd, rs1, rs2, funct, rs1_n) {         \
    register uint64_t rs1_ asm ("x" # rs1_n) = (uint64_t) rs1;          \
    asm volatile (                                                      \
        ".word " STR(CUSTOMX(X, 0, 1, 0, rd, rs1_n, rs2, funct)) "\n\t" \
        :: [_rs1] "r" (rs1_));                                          \
  }

#define ROCC_INSTRUCTION_I_I_I(X, rd, rs1, rs2, funct) {                 \
    asm volatile (                                                       \
        ".word " STR(CUSTOMX(X, 0, 0, 0, rd, rs1, rs2, funct)) "\n\t" ); \
  }

// SZx specific RoCC commands
#define SZX_CONFIG        0
#define SZX_SET_RADIUS    1
#define SZX_COMPRESS      2
#define SZX_COMPRESS_MULTI 3
#define SZX_LOAD_DATA     4  // New command to load data to scratchpad
#define SZX_GET_RESULT    5  // New command to get compressed result
#define SZX_LOAD_BLOCK    6  // New command for bulk block loading

// Helper functions for SZx RoCC operations
static inline void szx_config(float error_bound, float median_value) {
    union { float f; uint32_t i; } u1, u2;
    u1.f = error_bound;
    u2.f = median_value;
    ROCC_INSTRUCTION_SS(0, u1.i, u2.i, SZX_CONFIG);
}

static inline void szx_set_radius(float radius) {
    union { float f; uint32_t i; } u;
    u.f = radius;
    ROCC_INSTRUCTION_SS(0, u.i, 0, SZX_SET_RADIUS);
}

// New function to load data to RoCC scratchpad
static inline void szx_load_data(uint32_t data_value, uint32_t index) {
    ROCC_INSTRUCTION_I_R_R(0, 0, data_value, index, SZX_LOAD_DATA, 10, 11);
}

// New function for bulk block loading
static inline void szx_load_block_bulk(uint32_t block_start_addr, uint32_t block_size) {
    ROCC_INSTRUCTION_I_R_R(0, 0, block_start_addr, block_size, SZX_LOAD_BLOCK, 10, 11);
}

// New function to get compressed result
static inline uint32_t szx_get_result() {
    uint32_t result;
    ROCC_INSTRUCTION_DSS(0, result, 0, 0, SZX_GET_RESULT);
    return result;
}

static inline uint32_t szx_compress(float* input_data, uint8_t* output_data) {
    uint64_t input_addr = (uint64_t)input_data;
    uint64_t output_addr = (uint64_t)output_data;
    uint32_t compressed_size;
    ROCC_INSTRUCTION_DSS(0, compressed_size, input_addr, output_addr, SZX_COMPRESS);
    return compressed_size;
}

/* Compress data already loaded into HW scratchpad via szx_load_block_bulk() and return compressed size in bytes */
static inline uint32_t szx_compress_from_scratchpad(uint8_t* output_data) {
    uint32_t temp;
    ROCC_INSTRUCTION_DSS(0, temp, 0, 0, SZX_COMPRESS);
  
    uint32_t compressed_size;
    ROCC_INSTRUCTION_DSS(0, compressed_size, 0, 0, SZX_GET_RESULT);
    return compressed_size;
}

static inline uint32_t szx_compress_multi(float* input_data, uint8_t* output_data) {
    uint64_t input_addr = (uint64_t)input_data;
    uint64_t output_addr = (uint64_t)output_data;
    uint32_t compressed_size;
    ROCC_INSTRUCTION_DSS(0, compressed_size, input_addr, output_addr, SZX_COMPRESS_MULTI);
    return compressed_size;
}

#endif  // SRC_MAIN_C_ROCC_H
