#include <Windows.h>

struct timespec
{
  uint64_t tv_sec;
  uint64_t tv_nsec;
};

inline void
FILETIMEtoLARGE_INTEGER(const FILETIME& f, LARGE_INTEGER& t)
{
  t.QuadPart = 0LL;
  t.QuadPart = f.dwHighDateTime;
  t.QuadPart <<= 32;
  t.QuadPart |= f.dwLowDateTime;  
}

LARGE_INTEGER
getFILETIMEoffset()
{
  SYSTEMTIME s;
  FILETIME f;
  LARGE_INTEGER t;

  s.wYear = 1970;
  s.wMonth = 1;
  s.wDay = 1;
  s.wHour = 0;
  s.wMinute = 0;
  s.wSecond = 0;
  s.wMilliseconds = 0;
  SystemTimeToFileTime(&s, &f);
  FILETIMEtoLARGE_INTEGER(f,t);
  return (t);
}

const uint64_t HUNDRED_MILLION = 100000000;

int
clock_gettime(int X, struct timespec *tv)
{
  LARGE_INTEGER           t;
  FILETIME            f;
  double                  nanoseconds;
  static LARGE_INTEGER    offset;
  static double           frequencyToNanoseconds;
  static int              initialized = 0;
  static BOOL             usePerformanceCounter = 0;
  static LARGE_INTEGER    timestart;

  if (!initialized) {
    initialized = 1;
    printf("USING SYSTEM TIME\n");
    offset = getFILETIMEoffset();
    frequencyToNanoseconds = 100.0;
  }

  GetSystemTimeAsFileTime(&f);
  FILETIMEtoLARGE_INTEGER(f,t);

  // subtract the unknown offset of the performance counter
  t.QuadPart -= offset.QuadPart;
  // add on the known system time offset from the start of the program
  nanoseconds = (double)t.QuadPart * frequencyToNanoseconds;
  t.QuadPart = (long long)floor(nanoseconds);
  //t.QuadPart += timestart.QuadPart;
  tv->tv_sec = t.QuadPart / BILLION;
  tv->tv_nsec = t.QuadPart % BILLION;
  return (0);
}

