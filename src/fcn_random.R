# make sampling function to include at least one accession when n < 10
sample_up <- function(.data, frac) {
  sample_n(.data, ceiling({
    {
      frac
    }
  } * n()))
}