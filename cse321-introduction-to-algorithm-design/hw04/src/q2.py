import random

def partition(arr, first, last):
    piv = arr[first]
    right = first
    left = last

    while right < left:
        while right <= last and arr[right] <= piv:
            right += 1
        while left >= first and arr[left] > piv:
            left -= 1
        if right < left:
            arr[right], arr[left] = arr[left], arr[right]
    arr[first], arr[left] = arr[left], arr[first]
    return left

def selectHelper(arr, first, last, i):
    if first >= last:
        return arr[first]
    
    p = partition(arr, first, last)

    if p == i:  
        return arr[i]
    elif p > i:
        return selectHelper(arr, first, p - 1, i)
    else:
        return selectHelper(arr, p + 1, last, i)

def select(arr, i):
    return selectHelper(arr, 0, len(arr) - 1, i)

def median(arr):
    size = len(arr)
    i = size // 2

    if size % 2 == 0:
        return (select(arr, i - 1) + select(arr, i)) / 2 
    else:
        return select(arr, i) 

def createRandom(n, min = 0, max = 100):
    v = []
    for i in range(n):
        v.append(random.randint(min, max))
    return v

N = 5
A = createRandom(N)

# print(f"ith smallest element of A is {select(A, i)} for i = {i}")
print(f"{A} size: {len(A)}")
print(f"median of of A is {median(A)}")
print(f"{A} size: {len(A)}")
# for testing the result apply quick sort
A.sort()
print(f"{A} size: {len(A)}")