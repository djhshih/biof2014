library(ggplot2)

# outcome is tail or head
outcomes <- c("T", "H");

# probabilities of each outcome
#outcomes.prob <- c(0.3, 0.7);
outcomes.prob <- c(0.5, 0.5);

# ensure that probs sum to 1
outcomes.prob <- outcomes.prob / sum(outcomes.prob);

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

sample.space <- enumerate(outcomes, n.trials);

# probability of each element in the sample space
sample.space.prob <- unlist(lapply(
  enumerate(outcomes.prob, n.trials),
  function(probs) {
    # probs contain the probability of each trial
    # obtain the probability for the sequence by multiplying probs together
    prod(probs)
  }
));

sum(sample.space.prob)

# draw a sample s
s <- sample(outcomes, n.trials, replace=TRUE, prob=outcomes.prob);

sum(unlist(lapply(sample.space, function(r) all(r == s))))

# we define random variable X as the number of heads

# map each element of the sample space onto the domain of X
sample.space.x <- unlist(lapply(sample.space, function(r) sum(r == "H")));

# domain of random variable X
domain.x <- unique(sample.space.x);

# induced probability function on random variable X
prob.x <- unlist(lapply(domain.x,
  function(x) {
    idx <- sample.space.x == x;
    sum(sample.space.prob[idx])
  }
));

print(prob.x)

ggplot(data.frame(x=domain.x, y=prob.x), aes(x, y)) + theme_classic() +
  geom_col()

