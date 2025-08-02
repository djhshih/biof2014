draw_cards <- function(n) {
   sample(c(1:9, rep(0, 4)), n)
}

hand_value <- function(hand) {
   sum(hand) %% 10
}

odds_ratio <- function(prob1, prob2) {
   prob1 * (1 - prob2) / ((1 - prob1) * prob2)
}

norm <- function(probs) {
   probs / sum(probs)
}

simulate_game <- function(banker.strategy=0) {

   cards <- draw_cards(4);
   player.hand <- cards[c(1, 3)];
   banker.hand <- cards[c(2, 4)];

   min.natural <- 8;

   player.value <- hand_value(player.hand);
   banker.value <- hand_value(banker.hand);

   natural <- player.value >= min.natural ||
      banker.value >= min.natural;

   if (!natural) {
      # basic player strategy
      if (player.value <= 5) {
         player.hand <- c(player.hand, draw_cards(1));
         player.value <- hand_value(player.hand);
      }

      banker.hits <- FALSE;

      if (banker.strategy == 0) {
         # basic banker strategy
         if (banker.value <= 5) {
            banker.hits <- TRUE;
         }
      } else if (banker.strategy == 1) {
         # banker strategy in Baccarat punto banco
         # assumes that first two cards are dealt face-down
         if (length(player.hand) == 2) {
            if (banker.value <= 5) {
               banker.hits <- TRUE;
            }
         } else {
            if (player.hand[3] %in% c(0, 1, 9)) {
               if (banker.value <= 3) {
                  banker.hits <- TRUE;
               }
            } else if (player.hand[3] %in% 2:3) {
               if (banker.value <= 4) {
                  banker.hits <- TRUE;
               }
            } else if (player.hand[3] %in% 4:5) {
               if (banker.value <= 5) {
                  banker.hits <- TRUE;
               }
            } else if (player.hand[3] %in% 6:7) {
               if (banker.value <= 6) {
                  banker.hits <- TRUE;
               }
            } else if (player.hand[3] == 8) {
               if (banker.value <= 2) {
                  banker.hits <- TRUE;
               }
            }
         }
      } else {
         # banker strategy when all cards are dealt face-up
         if (banker.value < player.value) {
            banker.hits <- TRUE;
         } else if (banker.value == player.value && banker.value <= 5) {
            banker.hits <- TRUE;
         }
      }

      if (banker.hits) {
         banker.hand <- c(banker.hand, draw_cards(1));
         banker.value <- hand_value(banker.hand);
      }
   }

   if (player.value > banker.value) {
      winner <- 1;
   } else if (banker.value > player.value) {
      winner <- 2;
   } else {
      winner <- 0;
   }

   list(player = player.hand, banker = banker.hand, winner = winner)
}

#set.seed(1234);

commissions <- c(0, 0.05);

results0 <- lapply(1:100000, function(b) simulate_game(banker.strategy=0));
winners0 <- vapply(results0, function(x) x$winner, 0);
probs0 <- prop.table(table(winners0))
odds_ratio(probs0[3], probs0[2])
norm(probs0[2:3]) * (1 - commissions)

results1 <- lapply(1:100000, function(b) simulate_game(banker.strategy=1));
winners1 <- vapply(results1, function(x) x$winner, 0); probs1 <- prop.table(table(winners1))
odds_ratio(probs1[3], probs1[2])
norm(probs1[2:3]) * (1 - commissions)

results2 <- lapply(1:100000, function(b) simulate_game(banker.strategy=2));
winners2 <- vapply(results2, function(x) x$winner, 0);
probs2 <- prop.table(table(winners2))
odds_ratio(probs2[3], probs2[2])
norm(probs2[2:3]) * (1 - commissions)

