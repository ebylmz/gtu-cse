def exp(a, n):
    return expHelper(1, a, n)

def expHelper(y, a, n):
    if n == 0:
        return y
    elif n % 2 == 0:
        return expHelper(y, a * a, n / 2)
    else:
        return expHelper(a * y, a * a, (n - 1) / 2)

a = int(input("base: "))
n = int(input("exponent: "))
print(f"result: {a}^{n}: {exp(a, n)}")