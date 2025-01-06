library(ggplot2)

# outcome is tail or head
outcomes <- c("T", "H");

# probabilities of each outcome
probs.outcome <- c(0.3, 0.7);

# ensure that probs sum to 1
probs.outcome <- probs.outcome / sum(probs.outcome);

# number of coin flips in a sequence
n.trials <- 10;

# Enumerate the sample space of a sequence of n trials,
# given the outcomes of individual trials,
# returning the sample space
enumerate <- function(outcomes, n) {
  if (n == 1) {
    as.list(outcomes)
  } else {
    branches <- enumerate(outcomes, n - 1);
    unlist(
      lapply(branches,
        function(b) {
          lapply(outcomes, function(o) c(b, o))
        }
      ),
      recursive = FALSE
    )
  }
}

# sample space
S <- enumerate(outcomes, n.trials);

# probability of each element in sample space S
probs.S <- unlist(lapply(
  enumerate(probs.outcome, n.trials),
  function(probs) {
    # probs contain the probability of trials in a sequence
    # obtain the probability for the sequence by multiplying probs together
    prod(probs)
  }
));

sum(probs.S)

# we define random variable X as the number of heads
X <- function(s) {
  sum(s == "H")
}

# map each element of the sample space onto the domain of X
values.x <- unlist(lapply(S, X));

# domain of random variable X
domain.x <- unique(values.x);

# induced probability function on random variable X
probs.x <- unlist(lapply(domain.x,
  function(x) {
    # index of elements in the sample space that correspond to realization x
    # this index is used to extract the samples that correspond to 
    # the event where X = x
    idx <- values.x == x;
    sum(probs.S[idx])
  }
));

print(probs.x)

ggplot(data.frame(x=domain.x, y=probs.x), aes(x, y)) + theme_classic() +
  geom_col()


# draw a sample s
s <- sample(outcomes, n.trials, replace=TRUE, prob=probs.outcome);

# find the index of sample s in the sample space
s.idx <- which(unlist(lapply(S, function(s.i) all(s.i == s))));
probs.S[s.idx]

# map sample s of sample space to realization x
X(s)



# Remarks
# 1. Uniform probability in the sample space induced a non-uniform probability
#    distribution on X (number of heads).
# 2. Mathematical functions can be represented as vectors.


# Questions
# 1. When we are drawing coin flips, why do we sample with replacement?
# 2. How does changing the probability function on the sample space change
#    the induced probability function on X?
# 3. How can we sample the random variable X directly? This can be implemented
#    in one-line of code.


# Bonus questions
# 1. As the number of trials increases, can we still calculate `prob.x`
#    accurately? If not, modify the code to make the calculation of
#    `prob.x` more accurate.

