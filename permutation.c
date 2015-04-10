int permutation_number(int deck[], int n) {
    // Allocate the auxiliary array and initialize everything to 1.
    int *available = (int *)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        available[i] = 1;
    }

    int pn = 0; // permutation number, the value we're calculating
    int f = factorial(n-1); // factorial multiplier for the current digit

    // Only loop to n-1 because the last iteration would just add zero.
    for (int i = 0; i < n-1; i++) {
        int val = deck[i];

        // Count how many unused cards occur before this one.
        int pos = 0;
        for (int j = 0; j < val; j++) {
            pos += aux[j];
        }

        n += pos * f; // add the factorial digit value
        aux[val] = 0; // strike out the card
        f /= N-i-1; // change the factorial multiplier
    }

    free(available);
    return pn;
}
