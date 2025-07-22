#include <stdio.h>
#include <stdlib.h>
#include <math.h> // for abs()

#define MAX_SIZE 100

int readInput(const char *filename, int *N, int *numbers) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        return -1;
    }

    // Read N
    if (fscanf(fp, "%d", N) != 1) {
        fprintf(stderr, "Error reading N from file\n");
        fclose(fp);
        return -1;
    }

    // Read numbers
    if (*N > MAX_SIZE) {
        fprintf(stderr, "Number of elements exceeds maximum size (%d)\n", MAX_SIZE);
        fclose(fp);
        return -1;
    }
    for (int i = 0; i < *N; i++) {
        if (fscanf(fp, "%d", &numbers[i]) != 1) {
            fprintf(stderr, "Error reading number at index %d\n", i);
            fclose(fp);
            return -1;
        }
    }

    fclose(fp);
    return 0;
}

// Optimized approach to generate all contiguous subsequences
void allContiguousSubsequences(int *numbers, int N, int current_index, int *subsequence, int subsequence_size, int **all_subsequences, int *total_subsequences) {
    if (current_index == N) {
        // Add the current subsequence
        all_subsequences[*total_subsequences] = (int *)malloc(subsequence_size * sizeof(int));
        memcpy(all_subsequences[*total_subsequences], subsequence, subsequence_size * sizeof(int));
        (*total_subsequences)++;
        return;
    }

    // Include current element in subsequence
    subsequence[subsequence_size] = numbers[current_index];
    allContiguousSubsequences(numbers, N, current_index + 1, subsequence, subsequence_size + 1, all_subsequences, total_subsequences);

    // Exclude current element from subsequence
    allContiguousSubsequences(numbers, N, current_index + 1, subsequence, subsequence_size, all_subsequences, total_subsequences);
}

int sumOfSubsequences(int **subsequences, int total_subsequences) {
    int sum = 0;
    for (int i = 0; i < total_subsequences; i++) {
        int sub_sum = 0;
        for (int j = 0; subsequences[i][j] != 0; j++) {
            sub_sum += subsequences[i][j];
        }
        sum += sub_sum;
        free(subsequences[i]); // Deallocate memory after use
    }
    return sum;
}

int smallestAbsDiff(int *diffs, int total_diffs, int target_sum) {
    int min_diff = abs(diffs[0] - target_sum);
    for (int i = 1; i < total_diffs; i++) {
        min_diff = fmin(min_diff, abs(diffs[i] - target_sum));
    }
    return min_diff;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    int N, numbers[MAX_SIZE];
    if (readInput(argv[1], &N, numbers) != 0) {
        return 1;
    }

    // Allocate memory for all subsequences (dynamic allocation)
    int **all_subsequences = (int **)malloc(N * N * sizeof(int *));
    int total_subsequences = 0;

    allContiguousSubsequences(numbers, N, 0, (int *)malloc(N * sizeof(int)), 0, all_subsequences, &total_subsequences);

    // Calculate sum of all subsequences
    int sum_of_subsequences = sumOfSubsequences(all_subsequences, total_subsequences);

    // Calculate differences
