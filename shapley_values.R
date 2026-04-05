# Shapley Values Calculator for up to 4 players
#
# The Shapley value for player i is:
#   phi_i = sum over all subsets S not containing i of:
#           [|S|! * (n - |S| - 1)! / n!] * [v(S u {i}) - v(S)]

# =============================================================================
# STEP 1: Name your players
# =============================================================================
player_names <- c("Alice", "Bob", "Carol", "Dave")

# =============================================================================
# STEP 2: Define the value of every coalition
#
# List every non-empty subset and assign it a numeric worth.
# Keys are player names joined by "+", values are the coalition's worth.
# The empty coalition is always 0 and does not need to be listed.
#
# You only need entries for the coalitions that exist in your game
# (i.e. use the same names defined in player_names above).
# =============================================================================
coalition_values <- list(
  # --- Singletons ---
  "Alice"              = 0,
  "Bob"                = 0,
  "Carol"              = 0,
  "Dave"               = 0,

  # --- Pairs ---
  "Alice+Bob"          = 0.4,
  "Alice+Carol"        = 0.3,
  "Alice+Dave"         = 0.3,
  "Bob+Carol"          = 0.5,
  "Bob+Dave"           = 0.4,
  "Carol+Dave"         = 0.3,

  # --- Triples ---
  "Alice+Bob+Carol"    = 0.7,
  "Alice+Bob+Dave"     = 0.6,
  "Alice+Carol+Dave"   = 0.6,
  "Bob+Carol+Dave"     = 0.8,

  # --- Grand coalition ---
  "Alice+Bob+Carol+Dave" = 1.0
)

# =============================================================================
# STEP 3: Run (no edits needed below this line)
# =============================================================================

# Build the characteristic function v() from coalition_values
make_value_fn <- function(values, names) {
  function(indices) {
    if (length(indices) == 0) return(0)
    key <- paste(sort(names[indices]), collapse = "+")
    val <- values[[key]]
    if (is.null(val)) stop(sprintf("No value defined for coalition: '%s'", key))
    val
  }
}

shapley_values <- function(n, value_fn) {
  players <- seq_len(n)
  phi <- numeric(n)

  for (i in players) {
    others   <- players[players != i]
    n_others <- length(others)

    for (mask in 0:(2^n_others - 1)) {
      S      <- others[as.logical(intToBits(mask)[seq_len(n_others)])]
      s_size <- length(S)
      weight <- factorial(s_size) * factorial(n - s_size - 1) / factorial(n)

      phi[i] <- phi[i] + weight * (value_fn(c(S, i)) - value_fn(S))
    }
  }

  phi
}

n_players <- length(player_names)
if (n_players < 2 || n_players > 4) stop("Number of players must be between 2 and 4.")

v   <- make_value_fn(coalition_values, player_names)
phi <- shapley_values(n_players, v)

cat(sprintf("Shapley Values (%d players)\n", n_players))
cat(rep("-", 32), "\n", sep = "")
for (i in seq_len(n_players)) {
  cat(sprintf("  %-8s %.4f\n", paste0(player_names[i], ":"), phi[i]))
}
cat(rep("-", 32), "\n", sep = "")
cat(sprintf("  %-8s %.4f  (grand coalition: %.4f)\n",
            "Sum:", sum(phi), v(seq_len(n_players))))
