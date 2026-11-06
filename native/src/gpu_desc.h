#pragma once

#include "rhi/gpu.h"

/* What the table asks of the GPU. main.c adds the window and the run's switches. On Android, jni.c
 * checks this before SDL starts, so the launch check and gpu_init judge the phone the same way. */
extern const GpuDesc synth_gpu_desc;
