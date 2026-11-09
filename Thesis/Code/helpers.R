# Helpers shared by the analysis scripts.

# Sud Sardegna has no separate crime data before its 2016 boundary change, so it
# is folded into Cagliari wherever province names are harmonised.
fold_sud_sardegna <- function(x) ifelse(x == "Sud Sardegna", "Cagliari", x)

# fixest names the interaction coefficients "industry::<sector>:mafia_index".
# strip() reduces one to the bare sector label.
strip <- function(x) sub(":mafia_index$", "", sub("^industry::", "", x))
