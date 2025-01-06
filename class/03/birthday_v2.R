library(io)
library(ggplot2)
library(lubridate)

set.seed(1234);

# average daily births in England and Wales during th period from 1995 to 2014
d <- qread("uk-daily-births.csv");

d$dm <- d$date;
# 2024 contains 29-February
d$date <- dmy(paste0(d$date, "-2024"));

ggplot(d, aes(x=date, y=average)) +
	theme_classic() + scale_x_date(date_labels="%d %b") +
	geom_line() + xlab("") + ylab("average daily births")

# ---

# 29-Feb only occurs in leap years
# average daily births is calculated by the number of times that 29-Feb
# occurs, which removes the leap year effect

# however, for the birthday problem, we do want the leap year effect

# so, we will calculate the total daily births while accounting for leap years

start.year <- 1995;
end.year <- 2014;

leap.years <- intersect(seq(2024, 1995, by=-4), seq(start.year, end.year));

# number of years in recording period
n.years <- end.year - start.year + 1;

# number of leap years in recording period
n.leap.years <- length(leap.years);

# calculate total daily births
d$total <- d$average * n.years;
idx <- d$dm == "29-Feb";
d$total[idx] <- d$average[idx] * n.leap.years;

ggplot(d, aes(x=date, y=total)) +
	theme_classic() + scale_x_date(date_labels="%d %b") +
	geom_line() + xlab("") + ylab("total daily births")

# ---

# probability mass function of birthday
d$pmf <- d$total / sum(d$total);

ggplot(d, aes(x=date, y=pmf)) +
	theme_classic() + scale_x_date(date_labels="%d %b") +
	geom_line() + xlab("") + ylab("probability")

# ---

# Simulate birthdays and return the number of people who share the same birthday
# N  number of people
# probs  probability mass function of birthdays
simulate <- function(N, probs) {
  birthdays <- sample.int(length(probs), N, prob=probs, replace=TRUE);
  counts <- table(birthdays);
  max(counts)
}

N <- 30;
S <- 1000;  # number of simulation rounds
x <- vapply(1:S, function(s) simulate(N, d$pmf), 0);
event <- as.integer(x > 1);
estimate <- mean(event);
se <- sd(event) / sqrt(S);

conf_int <- function(estimate, se, alpha=0.05) {
	z <- qnorm(1 - alpha / 2);
	c(estimate - z*se, estimate + z*se)
}

estimate
conf_int(estimate, se)

# Probability of at least two people sharing the same birthday among
# N people, assuming J number of possible birthdays
prob_same_birthday <- function(N, J=365) {
	p1 <- 1;
	for (n in 0:(N-1)) {
		p1 <- p1 * (J - n) / J;
	}
	1 - p1
}

prob_same_birthday(N)

