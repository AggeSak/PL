#include <stdio.h>
#include <stdlib.h>
#include <math.h> // for abs()

#define MAX_SIZE 1000000


// Function to read input from a file
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

    // Validate N
    if (*N <= 1 || *N > MAX_SIZE) {
        fprintf(stderr, "Invalid number of elements: %d\n", *N);
        fclose(fp);
        return -1;
    }

    // Read numbers
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

// Function to calculate the sum of elements in an array
int sumOfArray(int *arr, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        sum += arr[i];
    }
    return sum;
}

// Function to calculate the smallest absolute difference for fair subsequences
int smallestAbsDiffFairSubsequence(int *numbers, int N) {
    int total_sum = sumOfArray(numbers, N);
    int min_diff = abs(numbers[0] - (total_sum - numbers[0]));

    for (int i = 1; i < N; i++) {
        for (int j = i; j < N; j++) {
            int sublist_sum = 0;
            for (int k = i; k <= j; k++) {
                sublist_sum += numbers[k];
            }
            int rest_sum = total_sum - sublist_sum;
            min_diff = fmin(min_diff, abs(sublist_sum - rest_sum));
        }
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

    // Calculate the smallest absolute difference for fair subsequences
    int min_diff = smallestAbsDiffFairSubsequence(numbers, N);

    printf("%d\n", min_diff);

    return 0;
}
