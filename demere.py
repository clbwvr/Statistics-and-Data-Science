import random
import math

# Probability of getting at least one "6" in four rolls of a single 6-sided die
def demere_first(sims):
	count = 0
	for i in range(sims):
		die1 = math.ceil(6 * random.uniform(0,1))
		die2 = math.ceil(6 * random.uniform(0,1))
		die3 = math.ceil(6 * random.uniform(0,1))
		die4 = math.ceil(6 * random.uniform(0,1))
		if(max(die1,die2,die3,die4) == 6):
			count += 1
	prob = count/sims
	se = math.sqrt(prob * (1 - prob) / sims)
	return("phat = {} | standard error = {} | n = {} | true = {}".format(prob,se,sims,.5177))

# Probability of at least one double-six in 24 throws of two dice,
def demere_second(sims):
	count = 0
	for i in range(sims):
		double_six = False
		for j in range(24):
			die1 = math.ceil(6 * random.uniform(0,1))
			die2 = math.ceil(6 * random.uniform(0,1))
			if double_six == False:
				double_six = (die1 == 6 and die2 == 6)
		if double_six:
			count += 1
	prob = count/sims
	se = math.sqrt(prob * (1-prob) / sims)
	return("phat = {} | standard error = {} | n = {} | true = {}".format(prob,se,sims,.4914))

print("Demere's First Problem:")
print(demere_first(100))
print(demere_first(1000))
print(demere_first(10000))
print()
print("Demere's Second Problem:")
print(demere_second(100))
print(demere_second(1000))
print(demere_second(10000))