def increasingConsecutiveSubarray(arr):
    n = len(arr)
    length = [1] * n
    maxLenght = 0

    for i in range(1, n):
        # check if arr[i] could be added on the previos subarray
        if arr[i] > arr[i - 1]:
            length[i] = length[i - 1] + 1
            if length[i] > maxLenght:
                maxLenght = length[i]
    return maxLenght

inputs = [
    [10, 22, 9, 33, 21, 50, 41, 60],
    [1, 4, 5, 2, 4, 3, 6, 7, 1, 2, 3, 4, 7],
    [1, 2, 3, 4, 1, 2, 3, 5, 2, 3, 4],
    [1, 2, 2, 3, 1, 4, 1, 2, 1, 4, 3, 5, 2, 4, 3, 5, 4]]

for i in range(len(inputs)):
    arr = inputs[i]
    print("arr: ", arr)
    print("result: len:", increasingConsecutiveSubarray(arr), "\n")