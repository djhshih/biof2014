library(ggplot2)

# outcome is tail or head
outcomes <- c("T", "H");

# probabilities of each outcome
prob <- c(0.3, 0.7);

# ensure that probs sum to 1
prob <- prob / sum(prob);

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

# draw a sample s
s <- sample(outcomes, n.trials, replace=TRUE, prob=prob);

sum(unlist(lapply(sample.space, function(r) all(r == s))))

# we define random variable X as the number of heads

# all values of X in the sample space
x.values <- unlist(lapply(sample.space, function(r) sum(r == "H")));

# domain of random variable X
X.domain <- unique(x.values);

# induced probability function on random variable X
ind.prob <- table(x.values) / length(x.values);

hist(x.values)

