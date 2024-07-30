# This module handles the process of using bootstrapping to generate confidence intervals for
# model coefficient difference estimates.

library(boot)

set.seed(1024)

# Load data from the previous step.
dec_data_t_subset_log <- read.csv("dec_data_t_subset_log.csv")
dec_data_t_subset_inv <- read.csv("dec_data_t_subset_inv.csv")
dec_data_t_subset_ide <- read.csv("dec_data_t_subset_ide.csv")

# Remove columns that we don't need.
dec_data_t_subset_log <- dec_data_t_subset_log[, c(-1, -2)]
dec_data_t_subset_inv <- dec_data_t_subset_inv[, c(-1, -2)]
dec_data_t_subset_ide <- dec_data_t_subset_ide[, c(-1, -2)]

# Fit an initial model.
fit <- glm("All_Time_PB ~ .", data=dec_data_t_subset_log, family=Gamma(link="log"))
# Get coefficient names.
coef_names <- names(coefficients(fit))

boot_iter_diff <- function(df_og, i, coef_idx_a, coef_idx_b, model_link) {
	# Select data.
	data_boot = df_og[i, ]
	# Fit model.
	fit <- glm("All_Time_PB ~ .", data=data_boot, family=Gamma(link=model_link))

	# Compute output.
	coef_out <- coefficients(fit)

	# Return the difference in means.
	return(coef_out[coef_idx_a] - coef_out[coef_idx_b])
}

# Bootstrapped confidence intervals.
boot_ci <- function(data, coef_idx_a, coef_idx_b, coef_names, model_link) {
	# Generate bootstrap object.
	boot.obj <- boot(data=data, statistic=boot_iter_diff, R=1000,
					 coef_idx_a=coef_idx_a, coef_idx_b=coef_idx_b, model_link=model_link)

	# Generate confidence intervals.
	confidence_intervals <- boot.ci(boot.obj, type=c("bca"))
	print(paste("Confidence interval for", coef_names[coef_idx_a], "-", coef_names[coef_idx_b]))
	print(confidence_intervals)
	return(confidence_intervals)
}

# Bootstrap matrix generator.
CI_bootstrap_matrix <- function(data, coef_names, event_idx_start=2, event_idx_end=11, model_link="log") {
	# Create the matrix that will store our coefficient values.
	n_coef <- event_idx_end - event_idx_start + 1
	coef_matrix <- matrix(
		0, n_coef, n_coef,
		dimnames=list(coef_names[event_idx_start:event_idx_end],
					  coef_names[event_idx_start:event_idx_end])
	)

	for (i in 1:n_coef) {
		for (j in 1:n_coef) {
			coef_idx_a <- event_idx_start + i - 1
			coef_idx_b <- event_idx_start + j - 1
			if (coef_idx_a != coef_idx_b) {
				# Construct confidence interval.
				ci_result <- boot_ci(
					data=data,
					coef_idx_a=coef_idx_a, coef_idx_b=coef_idx_b,
					coef_names=coef_names, model_link=model_link
				)

				# Grab bias-corrected accelerated confidence interval values.
				ci <- ci_result$bca

				print(paste("matrix idx: ", i, ", ", j, sep=""))
				print(paste("coef_idx: ", coef_idx_a, ", ", coef_idx_b, sep=""))
				print(ci)

				# If the lower bound is above 0, the whole interval is above 0 and the row coefficient
				# (corresponding to index `i`) is larger.
				if (ci[4] > 0) {
					coef_matrix[i, j] <- -10
				}
				# If the upper bound is below 0, the whole interval is below 0 and the row coefficient
				# is smaller.
				else if (ci[5] < 0) {
					coef_matrix[i, j] <- 10
				}
				# If zero is in the interval, neither is significantly larger than
				# the other at this alpha.
				else {
					coef_matrix[i, j] <- 0
				}
			}
		}
	}

	return(coef_matrix)
}

# boot_ci_test <- boot_ci(data=dec_data_t_subset_log, coef_idx_a=7, coef_idx_b=6, coef_names=coef_names, model_link="log")

# bootstrap_ci_matrix_test <- CI_bootstrap_matrix(
# 	data=dec_data_t_subset_log, coef_names=coef_names,
# 	event_idx_start=6, event_idx_end=9,
# 	model_link="log"
# )

# FSB.
boot_ci_matrix_log_fsb <- CI_bootstrap_matrix(
	data=dec_data_t_subset_log, coef_names=coef_names,
	event_idx_start=2, event_idx_end=11,
	model_link="log"
)
boot_ci_matrix_inv_fsb <- CI_bootstrap_matrix(
	data=dec_data_t_subset_inv, coef_names=coef_names,
	event_idx_start=2, event_idx_end=11,
	model_link="inverse"
)
boot_ci_matrix_ide_fsb <- CI_bootstrap_matrix(
	data=dec_data_t_subset_ide, coef_names=coef_names,
	event_idx_start=2, event_idx_end=11,
	model_link="identity"
)

# Deltas.
boot_ci_matrix_log_delta <- CI_bootstrap_matrix(
	data=dec_data_t_subset_log, coef_names=coef_names,
	event_idx_start=12, event_idx_end=21,
	model_link="log"
)
boot_ci_matrix_inv_delta <- CI_bootstrap_matrix(
	data=dec_data_t_subset_inv, coef_names=coef_names,
	event_idx_start=12, event_idx_end=21,
	model_link="inverse"
)
boot_ci_matrix_ide_delta <- CI_bootstrap_matrix(
	data=dec_data_t_subset_ide, coef_names=coef_names,
	event_idx_start=12, event_idx_end=21,
	model_link="identity"
)

write.csv(boot_ci_matrix_log_fsb, "boot_CI_matrix_log_fsb.csv")
write.csv(boot_ci_matrix_inv_fsb, "boot_CI_matrix_inv_fsb.csv")
write.csv(boot_ci_matrix_ide_fsb, "boot_CI_matrix_ide_fsb.csv")

write.csv(boot_ci_matrix_log_delta, "boot_CI_matrix_log_delta.csv")
write.csv(boot_ci_matrix_inv_delta, "boot_CI_matrix_inv_delta.csv")
write.csv(boot_ci_matrix_ide_delta, "boot_CI_matrix_ide_delta.csv")

