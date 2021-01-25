## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(CohortPlat)

## ---- cache = TRUE, echo = FALSE, message = FALSE, results = "asis", warning = FALSE----

plot_ptable_function <- function(.) {
  orig <- .
  DT::datatable(.,
    filter = "top",
    class = 'cell-border stripe',
    extensions = c('Buttons', 'Scroller', 'FixedColumns'),
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX = TRUE,
      fixedColumns = TRUE,
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)
    )
  ) %>%
  DT::formatRound(colnames(orig), digits = 3) %>%
  DT::formatStyle(
    tail(colnames(orig), n = 1),
    backgroundColor = DT::styleInterval(c(0.05), c('lightblue', 'bold')),
    fontWeight = 'bold'
  )
}

plot_table_function <- function(.) {
  orig <- .
  DT::datatable(.,
    filter = "top",
    class = 'cell-border stripe',
    extensions = c('Buttons', 'Scroller', 'FixedColumns'),
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX = TRUE,
      fixedColumns = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)
    )
  ) %>%
  DT::formatRound(colnames(orig), digits = 3)
}

  post_prob_bin <- function(n_exp, n_contr, resp_exp, resp_contr, delta,
                            a0_exp, b0_exp, a0_contr, b0_contr) {

    # in notation of diploma thesis, this calculates the probability P(P_e >= P_c + delta_sup)
    prob_sup <- stats::integrate(function(y) {
      stats::dbeta(y, a0_exp + resp_exp, b0_exp - resp_exp + n_exp) *
        stats::pbeta(y - delta, a0_contr + resp_contr, b0_contr - resp_contr + n_contr)
    }, delta, 1)$value

    # return posterior probability
    return(prob_sup)
  }


## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
B1 <- matrix(nrow = 1, ncol = 3)
B1[1,] <- c(0.10, 0.50, 1.00)

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
B2 <- matrix(nrow = 1, ncol = 2)
B2[1,] <- c(0.10, 0.50)

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
B3 <- matrix(nrow = 1, ncol = 3)
B3[1,] <- c(0.10, 0.50, 1.00)

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
B4 <- matrix(nrow = 1, ncol = 2)
B4[1,] <- c(0.10, 0.50)

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
P1 <- list(list(testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE), p_sup = 0.025, p_prom = 0, p_adj = "none"))

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
P2 <- list(list(testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE), p_fut = 0.5, p_adj = "none"))

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
P3 <- list(list(est = "RR", p_hat_sup = 1.2, p_hat_fut = 1, p_hat_prom = Inf))

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------
P4 <- list(list(est = "RR", ci = 0.95, p_hat_lower_sup = 1.2, p_hat_upper_fut = 1, p_hat_lower_prom = Inf))

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------

# Comparison Combo vs Mono Interim Analysis
Bayes_Sup1_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup1_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Combo vs Backbone Interim Analysis
Bayes_Sup2_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup2_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Mono vs Placebo Interim Analysis
Bayes_Sup3_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup3_Int[1,] <- c(0.00, 0.80, 1.00)
# Comparison Backbone vs Placebo Interim Analysis
Bayes_Sup4_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup4_Int[1,] <- c(0.00, 0.80, 1.00)

# Comparison Combo vs Mono Final Analysis
Bayes_Sup1_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup1_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Combo vs Backbone Final Analysis
Bayes_Sup2_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup2_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Mono vs Placebo Final Analysis
Bayes_Sup3_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup3_Fin[1,] <- c(0.05, 0.80, 1.00)
# Comparison Backbone vs Placebo Final Analysis
Bayes_Sup4_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup4_Fin[1,] <- c(0.05, 0.80, 1.00)

# Wrapup in package format
Bayes_Sup <- list(list(Bayes_Sup1_Int, Bayes_Sup2_Int, Bayes_Sup3_Int, Bayes_Sup4_Int),
                  list(Bayes_Sup1_Fin, Bayes_Sup2_Fin, Bayes_Sup3_Fin, Bayes_Sup4_Fin))

# Comparison Combo vs Mono
Bayes_Fut1 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut1[1,] <- c(0.00, 0.60)
# Comparison Combo vs Backbone
Bayes_Fut2 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut2[1,] <- c(0.00, 0.60)
# Comparison Mono vs Placebo
Bayes_Fut3 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut3[1,] <- c(0.00, 0.60)
# Comparison Backbone vs Placebo
Bayes_Fut4 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut4[1,] <- c(0.00, 0.60)
Bayes_Fut <- list(list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4),
                  list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4))

## ---- cache = TRUE, message = FALSE, results = "asis", warning = FALSE--------

# Comparison Combo vs Mono Interim Analysis
P_Sup1_Int <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.15, p_prom = 0))
# Comparison Combo vs Backbone Interim Analysis
P_Sup2_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Mono vs Placebo Interim Analysis
P_Sup3_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Backbone vs Placebo Interim Analysis
P_Sup4_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))

# Comparison Combo vs Mono Final Analysis
P_Sup1_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.05, p_prom = 0, p_adj = "B"))
# Comparison Combo vs Backbone Final Analysis
P_Sup2_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.05, p_prom = 0, p_adj = "B"))
# Comparison Mono vs Placebo Final Analysis
P_Sup3_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.05, p_prom = 0, p_adj = "B"))
# Comparison Backbone vs Placebo Final Analysis
P_Sup4_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.05, p_prom = 0, p_adj = "B"))

# Wrapup in package format
P_Sup <- list(list(P_Sup1_Int, P_Sup2_Int, P_Sup3_Int, P_Sup4_Int),
              list(P_Sup1_Fin, P_Sup2_Fin, P_Sup3_Fin, P_Sup4_Fin))

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------

# Set decision rules ----------------

# Comparison Combo vs Mono Interim Analysis
Bayes_Sup1_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup1_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Combo vs Backbone Interim Analysis
Bayes_Sup2_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup2_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Mono vs Placebo Interim Analysis
Bayes_Sup3_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup3_Int[1,] <- c(0.00, 0.80, 1.00)
# Comparison Backbone vs Placebo Interim Analysis
Bayes_Sup4_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup4_Int[1,] <- c(0.00, 0.80, 1.00)

# Comparison Combo vs Mono Final Analysis
Bayes_Sup1_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup1_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Combo vs Backbone Final Analysis
Bayes_Sup2_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup2_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Mono vs Placebo Final Analysis
Bayes_Sup3_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup3_Fin[1,] <- c(0.05, 0.80, 1.00)
# Comparison Backbone vs Placebo Final Analysis
Bayes_Sup4_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup4_Fin[1,] <- c(0.05, 0.80, 1.00)

# Wrapup in package format
Bayes_Sup <- list(list(Bayes_Sup1_Int, Bayes_Sup2_Int, Bayes_Sup3_Int, Bayes_Sup4_Int),
                  list(Bayes_Sup1_Fin, Bayes_Sup2_Fin, Bayes_Sup3_Fin, Bayes_Sup4_Fin))

# Comparison Combo vs Mono
Bayes_Fut1 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut1[1,] <- c(0.00, 0.60)
# Comparison Combo vs Backbone
Bayes_Fut2 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut2[1,] <- c(0.00, 0.60)
# Comparison Mono vs Placebo
Bayes_Fut3 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut3[1,] <- c(0.00, 0.60)
# Comparison Backbone vs Placebo
Bayes_Fut4 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut4[1,] <- c(0.00, 0.60)
Bayes_Fut <- list(list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4),
                  list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4))

# Furthermore set stopping for safety probability for every patient
safety_prob <- 0.0001

# For comparisons with Backbone and Placebo, how should data be pooled?
sharing_type <- "all"

# Which differences in response rates between 1:Combo vs. Mono/Back and 2:Mono/Back vs Plac should be considered a success?
target_rr <- c(0.10, 0.05, 1)

# Set cohort rules ------------

# What is the interim and final sample size for the cohorts?
n_int <- 50
n_fin <- 100

# What is the maximum number of cohorts that can be included in the platform (including starting cohorts)?
cohorts_max <- 5

# With what probability should a new cohort be added for every patient?
cohort_random <- 0.02


# Set simulation rules ----------

# Should response rates for the arms and the correlation structures be drawn randomly?
random <- TRUE

# We specify the absolute response rates
random_type <- "absolute"

# What are the possible response rates for the combination therapies and with what probabilities should they be drawn?
rr_comb <- c(0.35, 0.40, 0.45)
prob_comb_rr <- c(0.4, 0.4, 0.2)
# What are the possible response rates for the mono therapies and with what probabilities should they be drawn?
rr_mono <- c(0.15, 0.20, 0.25)
prob_mono_rr <- c(0.2, 0.4, 0.4)
# What are the possible response rates for the backbone therapies and with what probabilities should they be drawn?
rr_back <- c(0.15, 0.20, 0.25)
prob_back_rr <- c(0.3, 0.4, 0.3)
# What are the possible response rates for the placebos and with what probabilities should they be drawn?
rr_plac <- c(0.10, 0.12, 0.14)
prob_plac_rr <- c(0.25, 0.5, 0.25)

# How should response rates be transformed to four probabilities in multinomial sampling where the options are:
# 1) Biomarker:0, Histology:0, 2) Biomarker:1, Histology:0, 3) Biomarker:0, Histology:1, 4) Biomarker:1, Histology:1
# Choose values such that: 1) Specificity*(1-x), 2) (1-Specificity)*(1-x), 3) (1-Sensitivity)*x, 4) Sensitivity*x
# (Sensitivity and Specificity of Biomarker in prediciting Histology)
# In the following example therefore two cases, each with 50% probability:
# 1) Sensitivity = Specificity = 75%
# 2) Sensitivity = Specificity = 85%
rr_transform <- list(
  function(x) {return(c(0.75*(1 - x), (1-0.75)*(1-x), (1-0.75)*x, 0.75*x))},
  function(x) {return(c(0.85*(1 - x), (1-0.85)*(1-x), (1-0.85)*x, 0.85*x))}
)
prob_rr_transform <- c(0.5, 0.5)

# After how many identified successful combos should the trial stop?
sr_drugs_pos <- 1

# Should individual arm data be saved?
stage_data <- TRUE

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
set.seed(12)
run1 <- simulate_trial(
  n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, rr_mono = rr_mono, rr_back = rr_back, 
  rr_plac = rr_plac, rr_transform = rr_transform, random = random, random_type = random_type, 
  prob_comb_rr = prob_comb_rr, prob_mono_rr = prob_mono_rr, prob_back_rr = prob_back_rr,
  prob_plac_rr = prob_plac_rr, stage_data = stage_data, cohort_random = cohort_random, 
  cohorts_max = cohorts_max, sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, 
  sharing_type = sharing_type, safety_prob = safety_prob, Bayes_Sup = Bayes_Sup,
  Bayes_Fut = Bayes_Fut, prob_rr_transform = prob_rr_transform
)

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------

# Set decision rules ----------------

# Comparison Combo vs Mono Interim Analysis
P_Sup1_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Combo vs Backbone Interim Analysis
P_Sup2_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Mono vs Placebo Interim Analysis
P_Sup3_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Backbone vs Placebo Interim Analysis
P_Sup4_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))

# Comparison Combo vs Mono Final Analysis
P_Sup1_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
# Comparison Combo vs Backbone Final Analysis
P_Sup2_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
# Comparison Mono vs Placebo Final Analysis
P_Sup3_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
# Comparison Backbone vs Placebo Final Analysis
P_Sup4_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))

# Wrapup in package format
P_Sup <- list(list(P_Sup1_Int, P_Sup2_Int, P_Sup3_Int, P_Sup4_Int),
              list(P_Sup1_Fin, P_Sup2_Fin, P_Sup3_Fin, P_Sup4_Fin))

# Furthermore set stopping for safety probability for every patient
safety_prob <- 0

# For comparisons with Backbone and Placebo, how should data be pooled?
sharing_type <- "dynamic"

# Which differences in response rates between 1:Combo vs. Mono/Back and 2:Mono/Back vs Plac should be considered a success?
target_rr <- c(0.10, 0.05, 1)

# Set cohort rules ------------

# What is the interim sample size for the additional cohorts?
n_int <- 100
# What should be the final sample size?
n_fin <- 200

# What is the maximum number of cohorts that can be included in the platform (including starting cohorts)?
cohorts_max <- 5

# With what probability should a new cohort be added at every timestamp?
cohort_random <- 0.03

# What is the minimum number of iterations between the addition of new cohort?
cohort_offset <- 20

# Finish evaluating all cohorts
sr_drugs_pos <- Inf

# Set simulation rules ----------

# Should response rates for the arms and the correlation structures be drawn randomly?
random <- TRUE

# We specify the absolute response rates
random_type <- "absolute"

# What are the possible response rates for the combination therapies and with what probabilities should they be drawn?
rr_comb <- c(0.35, 0.40, 0.45)
prob_comb_rr <- c(0.4, 0.4, 0.2)
# What are the possible response rates for the mono therapies and with what probabilities should they be drawn?
rr_mono <- c(0.15, 0.20, 0.25)
prob_mono_rr <- c(0.2, 0.4, 0.4)
# What are the possible response rates for the backbone therapies and with what probabilities should they be drawn?
rr_back <- c(0.15, 0.20, 0.25)
prob_back_rr <- c(0.3, 0.4, 0.3)
# What are the possible response rates for the placebos and with what probabilities should they be drawn?
rr_plac <- c(0.10, 0.12, 0.14)
prob_plac_rr <- c(0.25, 0.5, 0.25)

# How should response rates be transformed to four probabilities in multinomial sampling where the options are:
# 1) Biomarker:0, Histology:0, 2) Biomarker:1, Histology:0, 3) Biomarker:0, Histology:1, 4) Biomarker:1, Histology:1
# Choose values such that: 1) Specificity*(1-x), 2) (1-Specificity)*(1-x), 3) (1-Sensitivity)*x, 4) Sensitivity*x
# (Sensitivity and Specificity of Biomarker in prediciting Histology)
# In the following example therefore two cases, each with 50% probability:
# 1) Sensitivity = Specificity = 75%
# 2) Sensitivity = Specificity = 85%
rr_transform <- list(
  function(x) {return(c(0.75*(1 - x), (1-0.75)*(1-x), (1-0.75)*x, 0.75*x))},
  function(x) {return(c(0.85*(1 - x), (1-0.85)*(1-x), (1-0.85)*x, 0.85*x))}
)
prob_rr_transform <- c(0.5, 0.5)

# Should individual arm data be saved?
stage_data <- TRUE

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
set.seed(23)
run2 <- simulate_trial(
  n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, rr_mono = rr_mono, rr_back = rr_back, 
  rr_plac = rr_plac, rr_transform = rr_transform, random = random, prob_comb_rr = prob_comb_rr, 
  random_type = random_type, prob_mono_rr = prob_mono_rr, prob_back_rr = prob_back_rr,
  prob_plac_rr = prob_plac_rr, stage_data = stage_data, cohort_random = cohort_random, 
  cohorts_max = cohorts_max, sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, 
  sharing_type = sharing_type, safety_prob = safety_prob, P_Sup = P_Sup, 
  prob_rr_transform = prob_rr_transform, cohort_offset = cohort_offset
)

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
#plot_trial(run1)

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
#plot_trial(run2, unit = "n")

## ---- cache = TRUE, message = FALSE-------------------------------------------

# Set decision rules ----------------

# Set decision rules ----------------

# Comparison Combo vs Mono Interim Analysis
Bayes_Sup1_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup1_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Combo vs Backbone Interim Analysis
Bayes_Sup2_Int <- matrix(nrow = 1, ncol = 3)
Bayes_Sup2_Int[1,] <- c(0.05, 0.80, 1.00)
# Comparison Mono vs Placebo Interim Analysis
Bayes_Sup3_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup3_Int[1,] <- c(0.00, 0.80, 1.00)
# Comparison Backbone vs Placebo Interim Analysis
Bayes_Sup4_Int<- matrix(nrow = 1, ncol = 3)
Bayes_Sup4_Int[1,] <- c(0.00, 0.80, 1.00)

# Comparison Combo vs Mono Final Analysis
Bayes_Sup1_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup1_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Combo vs Backbone Final Analysis
Bayes_Sup2_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup2_Fin[1,] <- c(0.10, 0.80, 1.00)
# Comparison Mono vs Placebo Final Analysis
Bayes_Sup3_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup3_Fin[1,] <- c(0.05, 0.80, 1.00)
# Comparison Backbone vs Placebo Final Analysis
Bayes_Sup4_Fin <- matrix(nrow = 2, ncol = 3)
Bayes_Sup4_Fin[1,] <- c(0.05, 0.80, 1.00)

# Wrapup in package format
Bayes_Sup <- list(list(Bayes_Sup1_Int, Bayes_Sup2_Int, Bayes_Sup3_Int, Bayes_Sup4_Int),
                  list(Bayes_Sup1_Fin, Bayes_Sup2_Fin, Bayes_Sup3_Fin, Bayes_Sup4_Fin))

# Comparison Combo vs Mono
Bayes_Fut1 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut1[1,] <- c(0.00, 0.60)
# Comparison Combo vs Backbone
Bayes_Fut2 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut2[1,] <- c(0.00, 0.60)
# Comparison Mono vs Placebo
Bayes_Fut3 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut3[1,] <- c(0.00, 0.60)
# Comparison Backbone vs Placebo
Bayes_Fut4 <- matrix(nrow = 1, ncol = 2)
Bayes_Fut4[1,] <- c(0.00, 0.60)
Bayes_Fut <- list(list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4),
                  list(Bayes_Fut1, Bayes_Fut2, Bayes_Fut3, Bayes_Fut4))

# Furthermore set stopping for safety probability for every patient
safety_prob <- 0.0001

# For comparisons with Backbone and Placebo, how should data be pooled?
sharing_type <- "all"

# Which differences in response rates between 1:Combo vs. Mono/Back and 2:Mono/Back vs Plac should be considered a success?
target_rr <- c(0.10, 0.05, 1)

# Set cohort rules ------------

# What is the interim and final sample size for the cohorts?
n_int <- 50
n_fin <- 100

# What is the maximum number of cohorts that can be included in the platform (including starting cohorts)?
cohorts_max <- 5

# With what probability should a new cohort be added for every patient?
cohort_random <- 0.02


# Set simulation rules ----------

# Should response rates for the arms and the correlation structures be drawn randomly?
random <- TRUE

# We specify the absolute response rates
random_type <- "absolute"

# What are the possible response rates for the combination therapies and with what probabilities should they be drawn?
rr_comb <- c(0.35, 0.40, 0.45)
prob_comb_rr <- c(0.4, 0.4, 0.2)
# What are the possible response rates for the mono therapies and with what probabilities should they be drawn?
rr_mono <- c(0.15, 0.20, 0.25)
prob_mono_rr <- c(0.2, 0.4, 0.4)
# What are the possible response rates for the backbone therapies and with what probabilities should they be drawn?
rr_back <- c(0.15, 0.20, 0.25)
prob_back_rr <- c(0.3, 0.4, 0.3)
# What are the possible response rates for the placebos and with what probabilities should they be drawn?
rr_plac <- c(0.10, 0.12, 0.14)
prob_plac_rr <- c(0.25, 0.5, 0.25)

# How should response rates be transformed to four probabilities in multinomial sampling where the options are:
# 1) Biomarker:0, Histology:0, 2) Biomarker:1, Histology:0, 3) Biomarker:0, Histology:1, 4) Biomarker:1, Histology:1
# Choose values such that: 1) Specificity*(1-x), 2) (1-Specificity)*(1-x), 3) (1-Sensitivity)*x, 4) Sensitivity*x
# (Sensitivity and Specificity of Biomarker in prediciting Histology)
# In the following example therefore two cases, each with 50% probability:
# 1) Sensitivity = Specificity = 75%
# 2) Sensitivity = Specificity = 85%
rr_transform <- list(
  function(x) {return(c(0.75*(1 - x), (1-0.75)*(1-x), (1-0.75)*x, 0.75*x))},
  function(x) {return(c(0.85*(1 - x), (1-0.85)*(1-x), (1-0.85)*x, 0.85*x))}
)
prob_rr_transform <- c(0.5, 0.5)

# After how many identified successful combos should the trial stop?
sr_drugs_pos <- 1

# Should individual arm data be saved?
stage_data <- TRUE

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
# Set specific parameters
# How many iterations should be performed?
iter <- 10
# On how many cores should the calculation be performed?
coresnum <- 1
# Should the result be saved as an Excel File?
save <- TRUE
# Under which path?
path <- "C:\\Users\\Elias\\Desktop\\"
# Under which filename?
filename <- "Testrun"
# Should result also be returned as list?
ret_list <- TRUE
# Should individual trial data be saved?
ret_trials <- FALSE
# Should stability plots be shown?
plot_ocs <- TRUE
set.seed(50)
 ocs1 <- trial_ocs(
  n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, rr_mono = rr_mono, rr_back = rr_back, 
  rr_plac = rr_plac, rr_transform = rr_transform, random = random, random_type = random_type, 
  prob_comb_rr = prob_comb_rr, prob_mono_rr = prob_mono_rr, prob_back_rr = prob_back_rr,
  prob_plac_rr = prob_plac_rr, stage_data = stage_data, cohort_random = cohort_random, 
  cohorts_max = cohorts_max, sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, 
  sharing_type = sharing_type, safety_prob = safety_prob, Bayes_Sup = Bayes_Sup,
  Bayes_Fut = Bayes_Fut, prob_rr_transform = prob_rr_transform,
  ret_trials = ret_trials, iter = iter, coresnum = coresnum, save = FALSE, path = path, 
  filename = filename, ret_list = ret_list, plot_ocs = plot_ocs
 )
#ocs1[[3]]

## ---- cache = TRUE, message = FALSE-------------------------------------------

# Set decision rules ----------------

# Comparison Combo vs Mono Interim Analysis
P_Sup1_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Combo vs Backbone Interim Analysis
P_Sup2_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
# Comparison Mono vs Placebo Interim Analysis
P_Sup3_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))
P_Sup4_Int <- list(list(testfun = NA, p_sup = NA, p_prom = NA))

# Comparison Combo vs Mono Final Analysis
P_Sup1_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
# Comparison Combo vs Backbone Final Analysis
P_Sup2_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
# Comparison Mono vs Placebo Final Analysis
P_Sup3_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))
P_Sup4_Fin <- list(list(
testfun = function(x) stats::prop.test(x, alternative = "less", correct = FALSE),
p_sup = 0.10, p_prom = 0, p_adj = "B"))

# Wrapup in package format
P_Sup <- list(list(P_Sup1_Int, P_Sup2_Int, P_Sup3_Int, P_Sup4_Int),
              list(P_Sup1_Fin, P_Sup2_Fin, P_Sup3_Fin, P_Sup4_Fin))

# Furthermore set stopping for safety probability for every patient
safety_prob <- 0

# For comparisons with Backbone and Placebo, how should data be pooled?
sharing_type <- "dynamic"

# Which differences in response rates between 1:Combo vs. Mono/Back and 2:Mono/Back vs Plac should be considered a success?
target_rr <- c(0.10, 0.05, 1)

# Set cohort rules ------------

# What is the interim sample size for the additional cohorts?
n_int <- 100
# What should be the final sample size?
n_fin <- 200

# What is the maximum number of cohorts that can be included in the platform (including starting cohorts)?
cohorts_max <- 5

# With what probability should a new cohort be added at every timestamp?
cohort_random <- 0.03

# What is the minimum number of iterations between the addition of new cohort?
cohort_offset <- 20

# Finish evaluating all cohorts
sr_drugs_pos <- Inf

# Set simulation rules ----------

# Should response rates for the arms and the correlation structures be drawn randomly?
random <- TRUE

# We specify the absolute response rates
random_type <- "absolute"

# What are the possible response rates for the combination therapies and with what probabilities should they be drawn?
rr_comb <- c(0.35, 0.40, 0.45)
prob_comb_rr <- c(0.4, 0.4, 0.2)
# What are the possible response rates for the mono therapies and with what probabilities should they be drawn?
rr_mono <- c(0.15, 0.20, 0.25)
prob_mono_rr <- c(0.2, 0.4, 0.4)
# What are the possible response rates for the backbone therapies and with what probabilities should they be drawn?
rr_back <- c(0.15, 0.20, 0.25)
prob_back_rr <- c(0.3, 0.4, 0.3)
# What are the possible response rates for the placebos and with what probabilities should they be drawn?
rr_plac <- c(0.10, 0.12, 0.14)
prob_plac_rr <- c(0.25, 0.5, 0.25)

# How should response rates be transformed to four probabilities in multinomial sampling where the options are:
# 1) Biomarker:0, Histology:0, 2) Biomarker:1, Histology:0, 3) Biomarker:0, Histology:1, 4) Biomarker:1, Histology:1
# Choose values such that: 1) Specificity*(1-x), 2) (1-Specificity)*(1-x), 3) (1-Sensitivity)*x, 4) Sensitivity*x
# (Sensitivity and Specificity of Biomarker in prediciting Histology)
# In the following example therefore two cases, each with 50% probability:
# 1) Sensitivity = Specificity = 75%
# 2) Sensitivity = Specificity = 85%
rr_transform <- list(
  function(x) {return(c(0.75*(1 - x), (1-0.75)*(1-x), (1-0.75)*x, 0.75*x))},
  function(x) {return(c(0.85*(1 - x), (1-0.85)*(1-x), (1-0.85)*x, 0.85*x))}
)
prob_rr_transform <- c(0.5, 0.5)

# Should individual arm data be saved?
stage_data <- TRUE

## ---- cache = TRUE, fig.width = 8, fig.height = 10, message = FALSE-----------
# Set specific parameters
# How many iterations should be performed?
iter <- 10
# On how many cores should the calculation be performed?
coresnum <- 1
# Should the result be saved as an Excel File?
save <- TRUE
# Under which path?
path <- "C:\\Users\\Elias\\Desktop\\"
# Under which filename?
filename <- "Testrun"
# Should result also be returned as list?
ret_list <- TRUE
# Should individual trial data be saved?
ret_trials <- FALSE
# Should stability plots be shown?
plot_ocs <- TRUE
# set.seed(50)
#  ocs1 <- trial_ocs(
#   n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, rr_mono = rr_mono, rr_back = rr_back, 
#   rr_plac = rr_plac, rr_transform = rr_transform, random = random, prob_comb_rr = prob_comb_rr, 
#   random_type = random_type, prob_mono_rr = prob_mono_rr, prob_back_rr = prob_back_rr,
#   prob_plac_rr = prob_plac_rr, stage_data = stage_data, cohort_random = cohort_random, 
#   cohorts_max = cohorts_max, sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, 
#   sharing_type = sharing_type, safety_prob = safety_prob, P_Sup = P_Sup, 
#   prob_rr_transform = prob_rr_transform, cohort_offset = cohort_offset, ret_trials = ret_trials,
#   iter = iter, coresnum = coresnum, save = save, path = path, filename = filename, ret_list = ret_list,
#   plot_ocs = plot_ocs
#  )
# ocs1[[3]]

