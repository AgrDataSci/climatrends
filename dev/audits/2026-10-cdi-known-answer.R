# Independent known-answer CDI regression probe.
# Expected to FAIL on master d0297df until existing PR #20 is merged.
# Run manually after applying PR #20:
# Rscript dev/audits/2026-10-cdi-known-answer.R
# Do not treat this as a passing test on the current audit branch.

stopifnot(requireNamespace("climatrends", quietly = TRUE))

tmax = c(32, 34, 36, 38)
tmin = c(12, 14, 16, 18)
tmean = (tmax + tmin) / 2
result = climatrends::crop_sensitive(
  tmax, tmin,
  cdi_mean.threshold = 23,
  cdi_max.threshold = 29
)
expected_mean = max(mean(tmean) - 23, 0)
expected_max = max(mean(tmax) - 29, 0)
stopifnot(isTRUE(all.equal(result$cdi_mean_23[[1]], expected_mean)))
stopifnot(isTRUE(all.equal(result$cdi_max_29[[1]], expected_max)))
stopifnot(expected_mean != expected_max)
cat("CDI known-answer check passed.\n")
print(sessionInfo())
