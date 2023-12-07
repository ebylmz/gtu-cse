# solution with divide-and-conquer approach
def maxProfitDAC(prices, lo, hi):
    if lo >= hi:
        return lo, lo, lo, lo

    # search most profitable days on halves
    mid = lo + (hi - lo) // 2
    lbuy, lsell, lmin, lmax = maxProfitDAC(prices, lo, mid)
    rbuy, rsell, rmin, rmax = maxProfitDAC(prices, mid + 1, hi)

    # min and max prices that encountered so far
    # this information will be used by the caller of this recursive call
    cmin = lmin if prices[lmin] < prices[rmin] else rmin
    cmax = lmax if prices[lmax] > prices[rmax] else rmax

    # max profit could came from left, right or mixture of them
    # by max price of right (sell) and min price of left (buy)
    lprofit = prices[lsell] - prices[lbuy]
    rprofit = prices[rsell] - prices[rbuy]
    mProfit = prices[rmax] - prices[lmin]
    maxProfit = max(mProfit, lprofit, rprofit)

    buy, sell = 0, 0
    if maxProfit == mProfit:
        buy, sell = cmin, cmax 
    elif maxProfit == lprofit:
        buy, sell = lbuy, lsell
    else:
        buy, sell = rbuy, rsell

    return buy, sell, cmin, cmax

# solution with non-divide-and-conquer approach
def maxProfitLinear(prices):
    mini = 0    # index of minimum value 
    buy = 0     
    sell = 1
    maxProfit = prices[sell] - prices[buy]

    for i in range(1, len(prices)):
        if prices[i] - prices[mini] > maxProfit:
            # more profitable days are found 
            maxProfit = prices[i] - prices[mini]
            sell = i
            buy = mini
        elif prices[i] < prices[mini]:
            # keep the minimum value to check if more profitable days are exist
            mini = i
    return buy, sell

inputs = [
    [10, 11, 10, 9, 8, 7, 9, 11],
    [10, 5, 100, 5, 110, 70, 80, 120],
    [10, 5, 100, 50, 110, 70, 80, 120, 200, 500, 100],
    [100, 110, 80, 90, 110, 70, 80, 80, 90]]

for i in range(len(inputs)):
    prices = inputs[i]
    buyDAC, sellDAC, _min, _max = maxProfitDAC(prices, 0, len(prices) - 1)
    buyLinear, sellLinear = maxProfitLinear(prices)
    print("prices: ", prices)
    print("result (DAC)    : Buy on day", buyDAC, "for", prices[buyDAC], "tl",
            "and sell on day", sellDAC, "for", prices[sellDAC], "tl.")
    print("result (Linear) : Buy on day", buyLinear, "for", prices[buyLinear], "tl",
            "and sell on day", sellLinear, "for", prices[sellLinear], "tl.\n")