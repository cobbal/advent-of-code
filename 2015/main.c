#include <sys/time.h>

#include "day-01/day-01.h"
#include "day-02/day-02.h"
#include "day-03/day-03.h"

int main(void) {
    daySolver days[] = {
        day01,
        day02,
        day03,
    };
    int dayCount = sizeof(days) / sizeof(*days);
    int failed = 0;
    for (int i = 0; i < dayCount; i++) {
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
