#include <cstdio>
#include <cstdlib>
#include <random>

// for some reason my Haskell doesn't have System.Random, but C++ stdlib does

int main(int argc, char **argv) {
    if(argc != 4 && argc != 2) {
        printf("Usage: %s <number of doubles> <lower bound> <upper bound>\n", argv[0]);
        return 1;
    }
    int n = atoi(argv[1]);
    int lower, upper;
    if(argc == 4) {
        lower = atoi(argv[2]);
        upper = atoi(argv[3]);
    } else {
        lower = 0;
        upper = 100;
    }
    std::mt19937_64 gen { std::random_device{}() };
    std::uniform_real_distribution<double> dist { static_cast<double>(lower), static_cast<double>(upper) };
    for(int i = 0; i < n; i++) {
        printf("%f\n", dist(gen));
        if(i % 100000 == 0) {
            double percent = static_cast<double>(i) / static_cast<double>(n) * 100.0;
            fprintf(stderr, "Status: %d/%d (%.2f%%)\n", i, n, percent);
        }
    }
}
