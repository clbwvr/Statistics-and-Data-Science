import random
import math

def mean(l):
	return(sum(l)/len(l))

def median(l):
    half = len(l) // 2
    l.sort()
    if not len(l) % 2:
        return (l[half - 1] + l[half]) / 2.0
    return l[half]

def online_variance(data):
    n = 0
    mean = 0.0
    M2 = 0.0     
    for element in data:
        n += 1
        delta = element - mean
        mean += delta / n
        M2 += delta*(element - mean)
    return M2

# Mean vs Median
def mean_vs_median(sims):
	mu = 67
	sigma = 8 
	x = []
	means = []
	medians = []
	for i in range(sims):
		for j in range(100):
			x.append(random.gauss(mu, sigma))
		means.append(mean(x))
		medians.append(median(x))
	varmeans = online_variance(means)
	varmedians = online_variance(medians)
	print(varmedians)
	print(varmeans)
	print(varmedians/varmeans)

mean_vs_median(100)