#include <sys/time.h>

#include "common/common.h"

extern daySolver day01;
extern daySolver day02;
extern daySolver day03;
extern daySolver day04;
extern daySolver day05;
extern daySolver day06;
extern daySolver day07;
extern daySolver day08;
extern daySolver day09;
extern daySolver day10;
extern daySolver day11;
extern daySolver day12;
extern daySolver day13;
extern daySolver day14;
extern daySolver day15;
extern daySolver day16;
extern daySolver day17;
extern daySolver day18;
extern daySolver day19;
daySolver day20;
daySolver day21;
daySolver day22;
daySolver day23;
daySolver day24;
daySolver day25;

int main(void) {
    daySolver days[] = {
        day01, day02, day03, day04, day05,
        day06, day07, day08, day09, day10,
        day11, day12, day13, day14, day15,
        day16, day17, day18, day19, day20,
        day21, day22, day23, day24, day25,
    };
    int dayCount = sizeof(days) / sizeof(*days);
    int failed = 0;
    for (int i = 0; i < dayCount; i++) {
        if (days[i].dayMain == nullptr) { continue; }
        if (!days[i].isQuick) { continue; }
        printf("=== day-%02d ===\n", days[i].number);
        struct timeval startTV, endTV;
        gettimeofday(&startTV, nullptr);
        failed += days[i].dayMain();
        gettimeofday(&endTV, nullptr);
        int64_t startTime = 1000000 * (int64_t) startTV.tv_sec + startTV.tv_usec;
        int64_t endTime = 1000000 * (int64_t) endTV.tv_sec + endTV.tv_usec;
        int64_t msTaken = (endTime - startTime) / 1000;
        printf("time: %lld.%03llds\n", msTaken / 1000, msTaken % 1000); 
    }
    return !!failed;
}

