draw_cards <- function(n) {
   sample(c(1:9, rep(0, 4)), n)
}

hand_value <- function(hand) {
   sum(hand) %% 10
}

simulate_game <- function() {
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

      # basic banker strategy
      if (banker.value <= 5) {
         banker.hand <- c(banker.hand, draw_cards(1));
         banker.value <- hand_value(banker.hand);
      }
   }

   if (player.value > banker.value) {
      winner <- "player";
   } else if (banker.value > player.value) {
      winner <- "banker";
   } else {
      winner <- "tie";
   }

   list(player = player.hand, banker = banker.hand, winner = winner)
}

#set.seed(1234);
results <- lapply(1:10, function(b) simulate_game());

