#pragma once

#include <x86intrin.h>
typedef long long tsc_t;

enum {
  SML_ALLOC_MALLOC_OBJECT,
  SML_ALLOC_FAST,
  SML_ALLOC_FIND_BITMAP,
  SML_ALLOC_FIND_SEGMENT,
  SML_ALLOC_FIND_SEGMENT_0,
  SML_ALLOC_INC_NUM_FILLED,
  SML_ALLOC_TRY_FIND_SEGMENT,
  SML_ALLOC_REQUEST_SEGMENT,
  SML_ALLOC_REQUEST_SEGMENT_LEAVE_INTERNAL,
  SML_ALLOC_REQUEST_SEGMENT_WAIT_FOR_VOTE,
  SML_ALLOC_REQUEST_SEGMENT_TRY_FIND_SEGMENT,
  SML_ALLOC_REQUEST_SEGMENT_VOTE_CONTINUE,
  SML_ALLOC_REQUEST_SEGMENT_MOVE_ALL_TO_FILLED,
  SML_ALLOC_REQUEST_SEGMENT_VOTE_ABORT,
  SML_ALLOC_REQUEST_SEGMENT_ENTER_INTERNAL,
  SML_ALLOC_WAIT_FOR_VOTE_LOCK,
  SML_ALLOC_WAIT_FOR_VOTE_WAIT_FOR_GC,
  SML_ALLOC_WAIT_FOR_VOTE_UNLOCK,
  SML_ALLOC_WAIT_FOR_GC_COND_SIGNAL,
  SML_ALLOC_WAIT_FOR_GC_COND_WAIT,
  SML_ALLOC_WAIT_FOR_GC_CLEANUP,
  SML_ALLOC_COLLECTOR_LOCK,
  SML_ALLOC_COLLECTOR_WAIT,
  SML_ALLOC_COLLECTOR_UNLOCK,
  SML_ALLOC_COLLECTOR_DO_GC,
  SML_ALLOC_COLLECTOR_LOCK2,
  SML_ALLOC_COLLECTOR_BROADCAST,
  SML_ALLOC_COLLECTOR_COUNT_VOTE,
  SML_ALLOC_COLLECTOR_MOVE_PARTIAL_TO_FILLED,
  SML_ALLOC_COLLECTOR_DO_GC2,
  SML_ALLOC_COLLECTOR_LOCK3,
  SML_ALLOC_COLLECTOR_BROADCAST2,
  SML_ALLOC_COLLECTOR_COUNT_VOTE2,
  SML_ALLOC_GC_CONTROL_GC,
  SML_ALLOC_GC_SYNC1,
  SML_ALLOC_GC_SYNC2,
  SML_ALLOC_GC_MARK,
  SML_ALLOC_GC_ASYNC,
  SML_ALLOC_GC_FINALIZER,
  SML_ALLOC_GC_COLLECTOR_ASYNC,
  SML_ALLOC_LAST
};

static inline tsc_t get_ns() {
  struct timespec ts[1];
  clock_gettime(CLOCK_REALTIME, ts);
  return ts->tv_sec * 1000000000 + ts->tv_nsec;
}

static inline tsc_t get_tsc() {
  return _rdtsc();
}

void sml_record_alloc_time(unsigned int sz, tsc_t t0, tsc_t t1, long evt_idx);
