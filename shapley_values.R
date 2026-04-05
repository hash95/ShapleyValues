# Shapley Values Calculator for up to 4 players
#
# The Shapley value for player i is:
#   phi_i = sum over all subsets S not containing i of:
#           [|S|! * (n - |S| - 1)! / n!] * [v(S u {i}) - v(S)]
#
# Usage:
#   1. Define your characteristic function v() below.
#   2. Set the number of players (2 to 4).
#   3. Run the script.

# ── Characteristic function ──────────────────────────────────────────────────
# v(coalition) must return the worth of the coalition.
# coalition is an integer vector, e.g. c(1,2) means players 1 and 2.
# v(integer(0)) must return 0 (empty coalition).
#
# Example: a simple game where the grand coalition earns 1,
#          any pair earns 0.5, and individuals earn 0.

v <- function(coalition) {
  n_members <- length(coalition)
  if (n_members == 0) return(0)
  if (n_members == 1) return(0)
  if (n_members == 2) return(0.5)
  if (n_members == 3) return(0.8)
  if (n_members == 4) return(1.0)
}

# ── Number of players (2 to 4) ───────────────────────────────────────────────
n_players <- 4

# ── Shapley value calculation ─────────────────────────────────────────────────
shapley_values <- function(n, value_fn) {
  if (n < 2 || n > 4) stop("Number of players must be between 2 and 4.")

  players <- seq_len(n)
  phi <- numeric(n)

  for (i in players) {
    others <- players[players != i]

    # Enumerate all subsets of 'others' (coalitions not containing i)
    n_others <- length(others)
    for (mask in 0:(2^n_others - 1)) {
      # Decode bitmask into a subset S
      S <- others[as.logical(intToBits(mask)[seq_len(n_others)])]

      s_size <- length(S)
      weight <- factorial(s_size) * factorial(n - s_size - 1) / factorial(n)

      marginal <- value_fn(c(S, i)) - value_fn(S)
      phi[i]   <- phi[i] + weight * marginal
    }
  }

  phi
}

# ── Run and display results ───────────────────────────────────────────────────
phi <- shapley_values(n_players, v)

cat(sprintf("Shapley Values (%d players)\n", n_players))
cat(rep("-", 30), "\n", sep = "")
for (i in seq_len(n_players)) {
  cat(sprintf("  Player %d: %.4f\n", i, phi[i]))
}
cat(rep("-", 30), "\n", sep = "")
cat(sprintf("  Sum:      %.4f  (grand coalition value: %.4f)\n",
            sum(phi), v(seq_len(n_players))))
