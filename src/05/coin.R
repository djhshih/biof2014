# Direct implementation for sampling a sequence of coin flips

set.seed(1337);

# number of coin flips in a sequence
n.trials <- 10;

# outcome is tail or head
outcomes <- c("T", "H");

# probabilities of each outcome
probs.outcome <- c(0.3, 0.7);

# ensure that probs sum to 1
probs.outcome <- probs.outcome / sum(probs.outcome);

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

# define random variable X as the number of heads
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

probs.x

library(ggplot2)
ggplot(data.frame(x=domain.x, y=probs.x), aes(x, y)) + theme_classic() +
  geom_col() + ylab("probability")


# draw a sample s
s <- sample(outcomes, n.trials, replace=TRUE, prob=probs.outcome);

# map sample s of sample space to realization x
X(s)

# find the index of sample s in the sample space
s.idx <- which(unlist(lapply(S, function(s.i) all(s.i == s))));
probs.S[s.idx]

