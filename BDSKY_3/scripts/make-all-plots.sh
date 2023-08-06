## Create plots and posterior summaries for Gauteng Province analyses
Rscript scripts/posterior-summary.R "GP" "GP_fixed_clock_0.3_epo_3"
Rscript scripts/posterior-estimate.R "GP" "GP_fixed_clock_0.3_epo_3"

Rscript scripts/posterior-summary.R "GP" "GP_fixed_clock_0.3_epo_4"
Rscript scripts/posterior-estimate.R "GP" "GP_fixed_clock_0.3_epo_4"

Rscript scripts/posterior-summary.R "GP" "GP_fixed_clock_1.1_epo_3"
Rscript scripts/posterior-estimate.R "GP" "GP_fixed_clock_1.1_epo_3"

Rscript scripts/posterior-summary.R "GP" "GP_fixed_clock_1.1_epo_4"
Rscript scripts/posterior-estimate.R "GP" "GP_fixed_clock_1.1_epo_4"

## Create plots and posterior summaries for South Africa and Botswana analyses
Rscript scripts/posterior-summary.R "RSA_BW" "RSA_BW_fixed_clock_0.75_epo_3"
Rscript scripts/posterior-estimate.R "RSA_BW" "RSA_BW_fixed_clock_0.75_epo_3"

Rscript scripts/posterior-summary.R "RSA_BW" "RSA_BW_fixed_clock_0.75_epo_4"
Rscript scripts/posterior-estimate.R "RSA_BW" "RSA_BW_fixed_clock_0.75_epo_4"

Rscript scripts/posterior-summary.R "RSA_BW" "RSA_BW_fixed_clock_1.2_epo_3"
Rscript scripts/posterior-estimate.R "RSA_BW" "RSA_BW_fixed_clock_1.2_epo_3"

Rscript scripts/posterior-summary.R "RSA_BW" "RSA_BW_fixed_clock_1.2_epo_4"
Rscript scripts/posterior-estimate.R "RSA_BW" "RSA_BW_fixed_clock_1.2_epo_4"

## Create plots and posterior summaries for sensitivity analyses on RSA_BW dataset
Rscript scripts/posterior-summary.R "sensitivity_analysis" "RSA_BW_fixed_clock_0.75_epo_4_inf_dur_7"
Rscript scripts/posterior-estimate.R "sensitivity_analysis" "RSA_BW_fixed_clock_0.75_epo_4_inf_dur_7" 7

Rscript scripts/posterior-summary.R "sensitivity_analysis" "RSA_BW_fixed_clock_0.75_epo_4_inf_dur_14"
Rscript scripts/posterior-estimate.R "sensitivity_analysis" "RSA_BW_fixed_clock_0.75_epo_4_inf_dur_14" 14