#include <stdio.h>
#include <stdlib.h>

int divisible_sum_pairs(int nums[], int size, int k) {
  int i, j, counter = 0;

  if (k <= 0)
    return 0;

  for (i = 0; i < size - 1; ++i) {
    for (j = i + 1; j < size; ++j) {
      if ((nums[i] + nums[j]) % k == 0) {
        ++counter;
        printf("%d: (%d, %d)\n", counter, nums[i], nums[j]);
      }
    }
  }
  return counter;
}

int main(void) {
  int i, size, k;
  int * nums;

  printf("size: ");
  scanf("%d", &size);
  printf("divider: ");
  scanf("%d", &k);

  if (size > 0) {
    nums = (int *) calloc(size, sizeof(int));

    printf("numbers: ");
    for (i = 0; i < size; ++i)
      scanf("%d", nums + i);

    printf("result: %d\n", divisible_sum_pairs(nums, size, k));

    free(nums);
  }
}  