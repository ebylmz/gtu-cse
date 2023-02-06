# compares two string and return the number of match from start to end
def compare(s1, s2):
    i = 0 
    n = min(len(s1), len(s2))
    while i < n and s1[i] == s2[i]:
        i = i + 1
    return i

# finds common prefix of the array string
def commonPrefix(arr, lo, hi):
    if lo == hi:
        return arr[lo]
    if lo < hi:
        # divide the array into halves and find the common prefix seperatly
        mid = lo + (hi - lo) // 2
        s1 = commonPrefix(arr, lo, mid)
        s2 = commonPrefix(arr, mid + 1, hi)
        # lastly compare the results of halves to find the common prefix of whole array
        i = compare(s1, s2)
        return s1[:i] if i > 0 else ""

inputs = [
    ["programmable", "programming", "programmer", "programmatic", "programmability"],
    ["compute", "compatible", "computer", "compare", "compactness"]]

for i in range(len(inputs)):
    arr = inputs[i]
    print("arr: ", arr)
    print("result: ", commonPrefix(arr, 0, len(arr) - 1), "\n")