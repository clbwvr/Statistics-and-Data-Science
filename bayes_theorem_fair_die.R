##### Detecting an Unfair Die with Bayes' Theorem

# Set the possible values of the number of consecutive "fives" observed from rolling a die
x = 1:20

# Shady casino has 3% loaded dice. The loaded dice show "one" 1/3 of the time.
# Notice that when prior = .5, the posterior given 5 starts off higher than .5. This is because
# if we didn't know anything else, getting a 5 points to it being loaded.

prob.fair <- .97
prob.loaded <- 1 - prob.fair
prob.one.given.fair <- 1/6
prob.one.given.loaded <- 1/3


# calculate the probability of the die being unfair given the observed number of consecutive "fives"
# P(H|D) = P(D|H)P(H)/P(D)

# likelihood: prob of getting x fives in a row with a loaded dice is p^x
likelihood <- prob.one.given.loaded^x
prior <- prob.loaded

# marginal found by total probability (P(A) = sum(P(A|B_i)))
marginal <- prob.loaded*prob.one.given.loaded^x + prob.fair*prob.one.given.fair^x

# posterior found by bayes theorem (posterior = prior * likelihood / marginal of given)
# export the plot as a PNG image file

posterior <- prior * likelihood / marginal

png('INSERT YOUR DIRECTORY PATH HERE/unfair die plot.png')

# plot the calculated probabilities
# notice that the "yaxt = 'n'" option suppresses the verticle axis; I want to add my own custom axis later
plot(x, posterior, col=ifelse(posterior > .5, "red", "blue"), ylab = 'P(Die is Fair | X)', xlab = 'X - Number of Consecutive Ones Observed', main = "Using Bayes' Theorem to Detect an Unfair Die")
14*14
        
# add a custom horizontal line to show where P(Die = Unfair | X = x) = 0.5
abline(0.5, 0, col = 'red', lty=2)
