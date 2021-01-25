#' Simulates the cohort trial.
#'
#' @param n_int                Sample size per cohort to conduct interim analysis
#'
#' @param n_fin                Sample size per cohort at final
#'
#' @param cohorts_start        Number of cohorts to start the platform with
#'
#' @param trial_struc          Trial Structure:
#'                             "all_plac" = all cohorts have placebo arm
#'
#'                             "no_plac" = no cohort has placebo arm
#'
#'                             "stop_post_mono" = all cohorts start with placebo arm, but after first mono has been declared successful,
#'                             newly enrolled cohorts have no more placebo
#'
#'                             "stop_post_back" = all cohorts start with placebo arm, but after first backbone has been declared successful,
#'                             newly enrolled cohorts have no more placebo
#'
#' @param rr_comb              Response rates of combination therapies
#'
#' @param rr_back              Response rates of backbone arms
#'
#' @param rr_mono              Response rate of mono therapies
#'
#' @param rr_plac              Response rate of the placebo
#'
#' @param rr_transform         Function transforming all the above response rates to a vector of four probabilities for the multinomial simulation
#'                             First element is probability of both failures. Second element is probability of biomarker success and histology failure.
#'                             Third element is probability of biomarker failure and histology success. Fourth element is probability of both success.
#'
#' @param random               Should the response rates of the arms be randomly drawn from rr_exp? Default is FALSE.
#'
#' @param random_type          How should the response rates be drawn randomly? Options are:
#'
#'                             "absolute": Specify absolute response rates that will be drawn with a certain probability
#'
#'                             "risk_difference": Specify absolute response rates for placebo which will be drawn randomly, plus specify vectors
#'                             for absolute treatment effects of mono therapies over placebo and for combo over the mono therapies.
#'
#'                             "risk_ratio": Specify absolute response rates for placebo which will be drawn randomly, plus specify vectors
#'                             for relative treatment effects of mono therapies over placebo and for combo over the mono therapies.
#'
#'                             "odds_ratios": Specify response rate for placebo, specify odds-ratios for mono therapies (via rr_back and rr_mono)
#'                             and respective probabilities. On top, specify interaction for the combination therapy via rr_comb with prob_rr_comb.
#'                             Set: odds_combo = odds_plac * or_mono1 * or_mono2 * rr_comb.
#'                             If rr_comb > 1 -> synergistic, if rr_comb = 1 -> additive. If rr_comb < 1 -> antagonistic.
#'                             Default is "NULL".
#'
#' @param prob_comb_rr         If random == TRUE, what are the probabilities with which the elements of rr_comb should be drawn?
#'
#' @param prob_back_rr         If random == TRUE, what are the probabilities with which the elements of rr_back should be drawn?
#'
#' @param prob_mono_rr         If random == TRUE, what are the probabilities with which the elements of rr_mono should be drawn?
#'
#' @param prob_plac_rr         If random == TRUE, what are the probabilities with which the elements of rr_plac should be drawn?
#'
#' @param prob_rr_transform    If random == TRUE, what are the probabilities with which the elements of rr_transform should be drawn?
#'
#' @param stage_data           Should individual stage data be passed along? Default is TRUE
#'
#' @param cohort_random        If not NULL, indicates that new arms/cohorts should be randomly started.
#'                             For every patient, there is a cohort_random probability that a new cohort will be started.
#'
#' @param cohorts_max          Maximum number of cohorts that are allowed to be added throughout the trial
#'
#' @param cohort_offset        Minimum number of patients between adding new cohorts
#'
#' @param sr_drugs_pos         Stopping rule for successful experimental arms; Default = 1
#'
#' @param sr_pats              Stopping rule for total number of patients; Default = cohorts_max * n_fin + error term based on randomization
#'
#' @param sr_first_pos         Stopping rule for first successful cohort; if TRUE, after first cohort was found to be successful, no further cohorts will be included
#'                             but cohorts will finish evaluating, unless other stopping rules reached prior. Default is FALSE.
#'
#' @param target_rr            What is target to declare a combo a positive? Vector of length 3 giving 1) the threshold by which
#'                             the combo needs to be better than the monos and 2) the threhsold by which the monos need to be better than the placebo.
#'                             The third element of the vector specifies the relation, choices are 1=="risk-difference", 2=="risk-ratio" and 3=="odds-ratio".
#'                             By default: c(0,0, "risk-difference").
#'
#' @param sharing_type         Which backbone and placebo data should be used for arm comparisons; Default is "all". Another option is "concurrent" or "dynamic" or "cohort".
#'
#' @param safety_prob          Probability for a safety stop after every patient
#'
#' @param ...                  Further arguments to be passed to decision function, such as decision making criteria
#'
#' @return List containing: Responses and patients on experimental and control arm, total treatment successes and failures and final p-value
#'
#' @examples
#'
#' random <- TRUE
#'
#' rr_comb <- c(0.25, 0.35, 0.4)
#' prob_comb_rr <- c(0.4, 0.4, 0.2)
#' rr_mono <- c(0.15, 0.20, 0.25)
#' prob_mono_rr <- c(0.2, 0.4, 0.4)
#' rr_back <- c(0.20, 0.25, 0.30)
#' prob_back_rr <- c(0.3, 0.4, 0.3)
#' rr_plac <- c(0.10, 0.12, 0.14)
#' prob_plac_rr <- c(0.25, 0.5, 0.25)
#'
# rr_transform <- list(
#   function(x) {return(c(0.75*(1 - x), (1-0.75)*(1-x), (1-0.75)*x, 0.75*x))},
#   function(x) {return(c(0.85*(1 - x), (1-0.85)*(1-x), (1-0.85)*x, 0.85*x))}
# )
# prob_rr_transform <- c(0.5, 0.5)
#'
#' cohorts_max <- 4
#' trial_struc <- "stop_post_back"
#' safety_prob <- 0
#' sharing_type <- "concurrent"
#' sr_drugs_pos <- 4
#' sr_first_pos <- TRUE
#' n_int <- 50
#' n_fin <- 100
#' stage_data <- TRUE
#' cohort_random <- 0.05
#' target_rr <- c(0,0,1)
#' cohort_offset <- 5
#' random_type <- "risk_difference"
#'
#' # Vergleich Combo vs Mono
#' Bayes_Sup1 <- matrix(nrow = 3, ncol = 3)
#' Bayes_Sup1[1,] <- c(0.00, 0.90, 1.00)
#' Bayes_Sup1[2,] <- c(0.05, 0.65, 1.00)
#' Bayes_Sup1[3,] <- c(0.10, 0.50, 1.00)
#' # Vergleich Combo vs Backbone
#' Bayes_Sup2 <- matrix(nrow = 3, ncol = 3)
#' Bayes_Sup2[1,] <- c(0.05, 0.80, 1.00)
#' Bayes_Sup2[2,] <- c(NA, NA, NA)
#' Bayes_Sup2[3,] <- c(NA, NA, NA)
#' # Vergleich Mono vs Placebo
#' Bayes_Sup3 <- matrix(nrow = 3, ncol = 3)
#' Bayes_Sup3[1,] <- c(0.00, 0.90, 1.00)
#' Bayes_Sup3[2,] <- c(0.05, 0.65, 1.00)
#' Bayes_Sup3[3,] <- c(NA, NA, NA)
#' Bayes_Sup4 <- matrix(nrow = 3, ncol = 3)
#' Bayes_Sup4[1,] <- c(0.00, 0.90, 1.00)
#' Bayes_Sup4[2,] <- c(0.05, 0.65, 1.00)
#' Bayes_Sup4[3,] <- c(NA, NA, NA)
#' Bayes_Sup <- list(list(Bayes_Sup1, Bayes_Sup2, Bayes_Sup3, Bayes_Sup4),
#'              list(Bayes_Sup1, Bayes_Sup2, Bayes_Sup3, Bayes_Sup4))
#'
# simulate_trial(
# n_int = n_int, n_fin = n_fin, trial_struc = trial_struc, random_type = random_type,
# rr_comb = rr_comb, rr_mono = rr_mono, rr_back = rr_back, rr_plac = rr_plac,
# rr_transform = rr_transform, random = random, prob_comb_rr = prob_comb_rr,
# prob_mono_rr = prob_mono_rr, prob_back_rr = prob_back_rr, prob_plac_rr = prob_plac_rr,
# stage_data = stage_data, cohort_random = cohort_random, cohorts_max = cohorts_max,
# sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, sharing_type = sharing_type,
# safety_prob = safety_prob, Bayes_Sup = Bayes_Sup, prob_rr_transform = prob_rr_transform,
# cohort_offset = cohort_offset, sr_first_pos = sr_first_pos
# )
#'
#' @export
simulate_trial <- function(n_int = 50, n_fin = 100, cohorts_start = 1, rr_comb, rr_mono, rr_back, rr_plac,
                           rr_transform, random_type = NULL, trial_struc = "all_plac", random = FALSE,
                           prob_comb_rr = NULL, prob_mono_rr = NULL, prob_back_rr = NULL,
                           prob_plac_rr = NULL, prob_rr_transform = prob_rr_transform, stage_data = TRUE,
                           cohort_random = NULL, cohorts_max = 4, sr_drugs_pos = 1,
                           sr_pats = cohorts_max * (n_fin + 3 * cohorts_max), sr_first_pos = FALSE,
                           target_rr = c(0,0,1), cohort_offset = 0, sharing_type = "all", safety_prob = 0, ...) {

  ##### Initialization #####

  # ------------------ Helper functions
  sample.vec <- function(x, ...) x[sample(length(x), ...)]


  # helper function check which cohort is left
  coh_left_check <- function(x) {
    if (x$decision[1] %in% c("none", "PROMISING", "CONTINUE") & x$decision[2] == "none") {
      ret <- TRUE
    } else {
      ret <- FALSE
    }
    return(ret)
  }

  # helper functino to create initial cohort
  create_cohort_initial <- function(trial_struc, cohorts_start, n_int, n_fin,
                                    rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec) {

    if (trial_struc == "no_plac") {

      res_list <- rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0),
                             rep(list(list(rr = NULL, resp_bio = NULL, resp_hist = NULL, n = NULL)), 3))),
                      cohorts_start)

      for (i in 1:cohorts_start) {
        names(res_list)[i] <- paste0("Cohort", i)
        names(res_list[[i]])[5:7] <- c("Comb", "Mono", "Back")
        res_list[[i]]$alloc_ratio <- c(1,1,1)
        if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}
        res_list[[i]]$n_thresh <- n_thresh_vec
        res_list[[i]][[5]]$rr <- rr_comb_vec[i]
        res_list[[i]][[6]]$rr <- rr_mono_vec[i]
        res_list[[i]][[7]]$rr <- rr_back_vec[i]
      }

    } else {

      res_list <- rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0),
                             rep(list(list(rr = NULL, resp_bio = NULL, resp_hist = NULL, n = NULL)), 4))),
                      cohorts_start)

      for (i in 1:cohorts_start) {
        names(res_list)[i] <- paste0("Cohort", i)
        names(res_list[[i]])[5:8] <- c("Comb", "Mono", "Back", "Plac")
        res_list[[i]]$alloc_ratio <- c(1,1,1,1)
        if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}
        res_list[[i]]$n_thresh <- n_thresh_vec
        res_list[[i]][[5]]$rr <- rr_comb_vec[i]
        res_list[[i]][[6]]$rr <- rr_mono_vec[i]
        res_list[[i]][[7]]$rr <- rr_back_vec[i]
        res_list[[i]][[8]]$rr <- rr_plac_vec[i]
      }

    }

    # Update allocation ratio
    if (cohorts_start > 1) {
      if (sharing_type != "cohort") {
        res_list <- update_alloc_ratio(res_list)
      }
    }

    return(res_list)

  }


  # helper function to create new cohort
  create_cohort_new <- function(res_list, plac, n_int, n_fin, sharing_type,
                                rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec) {

    if (n_int == n_fin) {n_thresh_vec <- c(Inf, n_int)}else{n_thresh_vec <- c(n_int, Inf)}

    if (plac) {

      new_list <- list(c(list(decision = rep("none", 2),
                              alloc_ratio = c(1,1,1,1),
                              n_thresh = n_thresh_vec,
                              start_n = sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
      ),
      rep(list(list(rr = NULL,
                    resp_bio = rep(NA, length(res_list[[1]][[5]]$n)),
                    resp_hist = rep(NA, length(res_list[[1]][[5]]$n)),
                    n = rep(NA, length(res_list[[1]][[5]]$n)))), 4)))

      names(new_list)[1] <- paste0("Cohort", length(res_list) + 1)
      names(new_list[[1]])[5:8] <- c("Comb", "Mono", "Back", "Plac")
      new_list[[1]][[5]]$rr <- rr_comb_vec[length(res_list) + 1]
      new_list[[1]][[6]]$rr <- rr_mono_vec[length(res_list) + 1]
      new_list[[1]][[7]]$rr <- rr_back_vec[length(res_list) + 1]
      new_list[[1]][[8]]$rr <- rr_plac_vec[length(res_list) + 1]

    } else {

      new_list <- list(c(list(decision = rep("none", 2),
                              alloc_ratio = c(1,1,1),
                              n_thresh = n_thresh_vec,
                              start_n = sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
      ),
      rep(list(list(rr = NULL,
                    resp_bio = rep(NA, length(res_list[[1]][[5]]$n)),
                    resp_hist = rep(NA, length(res_list[[1]][[5]]$n)),
                    n = rep(NA, length(res_list[[1]][[5]]$n)))), 3)))

      names(new_list)[1] <- paste0("Cohort", length(res_list) + 1)
      names(new_list[[1]])[5:7] <- c("Comb", "Mono", "Back")
      new_list[[1]][[5]]$rr <- rr_comb_vec[length(res_list) + 1]
      new_list[[1]][[6]]$rr <- rr_mono_vec[length(res_list) + 1]
      new_list[[1]][[7]]$rr <- rr_back_vec[length(res_list) + 1]

    }

    res_list <- c(res_list, new_list)

    # Update allocation ratio

    if (sharing_type != "cohort") {
      res_list <- update_alloc_ratio(res_list)
    }

    return(res_list)

  }


  # helper function to retreive final sample size
  final_n_cohort <- function(res_list) {
    res <- matrix(nrow = 4, ncol = length(res_list))
    for (i in 1:length(res_list)) {
      for (j in 1:length(res_list[[i]]$alloc_ratio)) {
        res[j, i] <- sum(res_list[[i]][[j+4]]$n, na.rm = TRUE)
      }
    }
    rownames(res) <- c("Combo", "Mono", "Backbone", "Placebo")
    colnames(res) <- paste0("Cohort", 1:length(res_list))
    return(res)
  }


  # helper function to check whether stopping rules are reached
  is_sr_reached <- function(res_list, sr_drugs_pos, sr_pats, expected) {
    ret <- 0
    positives <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "GO")
    if (positives >= sr_drugs_pos) {
      ret <- 1
    }
    if (sr_pats < expected) {
      if (sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE) > sr_pats) {
        ret <- 1
      }
    }
    return(ret)
  }


  # helper functions to compute sample sizes
  total_n <- function(x) {
  if ("Plac" %in% names(x)) {
    sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$n), na.rm = TRUE)
  } else {
    sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$n), na.rm = TRUE)
  }
  }


  total_rb <- function(x) {
    if ("Plac" %in% names(x)) {
      sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$resp_bio), na.rm = TRUE)
    } else {
      sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$resp_bio), na.rm = TRUE)
    }
  }

  total_rh <- function(x) {
    if ("Plac" %in% names(x)) {
      sum(sapply(x[c("Comb", "Back", "Mono", "Plac")], function(y) y$resp_hist), na.rm = TRUE)
    } else {
      sum(sapply(x[c("Comb", "Back", "Mono")], function(y) y$resp_hist), na.rm = TRUE)
    }
  }


  # helper function to update allocation ratios
  update_alloc_ratio <- function(res_list) {

    cohorts_left <- which(sapply(res_list, function(x) coh_left_check(x)))
    comb_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Comb", na.rm = TRUE)
    back_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Back", na.rm = TRUE)
    mono_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Mono", na.rm = TRUE)
    plac_numb <- sum(sapply(res_list[cohorts_left], function(x) names(x)[5:8]) == "Plac", na.rm = TRUE)

    for (i in cohorts_left) {
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        res_list[[i]]$alloc_ratio <- c(comb_numb, mono_numb, 1)
      } else {
        res_list[[i]]$alloc_ratio <- c(comb_numb, mono_numb, 1, 1)
      }
    }

    return(res_list)
  }



  # Check whether random experimental response rates.
  # If so, simulate, if not, then length must equal cohorts_max (including first cohort)
  if (random) {
    if (random_type == "absolute") {
      # Sample response rates for all possible arms
      rr_comb_vec <- sample.vec(rr_comb, cohorts_max, prob = prob_comb_rr, replace = TRUE)
      rr_back_vec <- sample.vec(rr_back, cohorts_max, prob = prob_back_rr, replace = TRUE)
      rr_mono_vec <- sample.vec(rr_mono, cohorts_max, prob = prob_mono_rr, replace = TRUE)
      rr_plac_vec <- sample.vec(rr_plac, cohorts_max, prob = prob_plac_rr, replace = TRUE)
    }

    if (random_type == "risk_difference") {
      rr_plac_vec <- sample.vec(rr_plac, cohorts_max, prob = prob_plac_rr, replace = TRUE)
      mono_add <- sample.vec(rr_mono, cohorts_max, prob = prob_mono_rr, replace = TRUE)
      back_add <- sample.vec(rr_back, cohorts_max, prob = prob_back_rr, replace = TRUE)
      rr_mono_vec <- pmin(rr_plac_vec + mono_add, 1)
      rr_back_vec <- pmin(rr_plac_vec + back_add, 1)
      comb_add <- sample.vec(rr_comb, cohorts_max, prob = prob_comb_rr, replace = TRUE)
      rr_comb_vec <- pmin(rr_plac_vec + back_add + mono_add + comb_add, 1)
    }

    if (random_type == "risk_ratio") {
      rr_plac_vec <- sample.vec(rr_plac, cohorts_max, prob = prob_plac_rr, replace = TRUE)
      mono_add <- sample.vec(rr_mono, cohorts_max, prob = prob_mono_rr, replace = TRUE)
      back_add <- sample.vec(rr_back, cohorts_max, prob = prob_back_rr, replace = TRUE)
      rr_mono_vec <- pmin(rr_plac_vec * mono_add, 1)
      rr_back_vec <- pmin(rr_plac_vec * back_add, 1)
      comb_add <- sample.vec(rr_comb, cohorts_max, prob = prob_comb_rr, replace = TRUE)
      rr_comb_vec <- pmin(rr_plac_vec * mono_add * back_add * comb_add, 1)
    }

    if (random_type == "odds_ratios") {
      odds_to_rr <- function(x) {x/(1+x)}
      rr_to_odds <- function(x) {x/(1-x)}
      # get placebo response rate
      rr_plac_vec <- sample.vec(rr_plac, cohorts_max, prob = prob_plac_rr, replace = TRUE)
      # get mono and backbone odds ratios
      mono_add_or <- sample.vec(rr_mono, cohorts_max, prob = prob_mono_rr, replace = TRUE)
      back_add_or <- sample.vec(rr_back, cohorts_max, prob = prob_back_rr, replace = TRUE)
      # compute mono and backbone odds
      odds_plac_vec <- rr_to_odds(rr_plac_vec)
      odds_mono_vec <- odds_plac_vec * mono_add_or
      odds_back_vec <- odds_plac_vec * back_add_or
      # sample combo odds "strength" (corresponds to either "a", "s" or "g")
      rr_comb_interaction <- sample.vec(rr_comb, cohorts_max, prob = prob_comb_rr, replace = TRUE)
      # get combo odds
      odds_comb_vec <- odds_plac_vec * mono_add_or * back_add_or * rr_comb_interaction
      # transfer odds to rr
      rr_mono_vec <- odds_to_rr(odds_mono_vec)
      rr_back_vec <- odds_to_rr(odds_back_vec)
      rr_comb_vec <- odds_to_rr(odds_comb_vec)
    }

    rr_transform_vec <- rr_transform[sample(1:length(rr_transform), cohorts_max, prob = prob_rr_transform, replace = TRUE)]

  } else {
    rr_comb_vec <- rep(rr_comb, cohorts_max)
    rr_back_vec <- rep(rr_back, cohorts_max)
    rr_mono_vec <- rep(rr_mono, cohorts_max)
    rr_plac_vec <- rep(rr_plac, cohorts_max)
    rr_transform_vec <- rr_transform[sample(1:length(rr_transform), cohorts_max, prob = 1, replace = TRUE)]
  }


  # initialize first vector of active cohorts
  cohorts_left <- 1:cohorts_start

  # dummy to indicate trial stop
  trial_stop <- 0

  # dummy for timestamp for first success
  first_success <- -1

  # Variable measuring patients since last cohort was added
  last_cohort_time <- 0

  # Initialize res_list
  res_list <- create_cohort_initial(trial_struc, cohorts_start, n_int, n_fin,
                                    rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec)

  Total_N_Vector <- NULL

  # Initialize indicators whether any combo or mono has been found successful
  comb_suc <- 0
  mono_suc <- 0
  back_suc <- 0

  ##### Running Simulations #####
  while (!trial_stop) {

    ##### Misc #####

    # Check whether allocation ratios need to be changed
    if (!identical(cohorts_left, which(sapply(res_list, function(x) coh_left_check(x)))) & sharing_type != "cohort") {
      res_list <- update_alloc_ratio(res_list)
    }

    # Check which cohorts are recruiting
    cohorts_left <- which(sapply(res_list, function(x) coh_left_check(x)))
    # Check which cohorts are finished
    cohorts_finished <- which(!sapply(res_list, function(x) coh_left_check(x)))

    ##### N and Resp #####

    patients_timestamp <- 0

    # Get new patients and responders for every cohorts
    for (i in cohorts_left) {
      f <- match.fun(rr_transform_vec[[i]])
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        for (j in 5:7) {
          # get sample sizes
          res_list[[i]][[j]]$n <- c(res_list[[i]][[j]]$n, res_list[[i]]$alloc_ratio[j-4])
          patients_timestamp <- patients_timestamp + res_list[[i]]$alloc_ratio[j-4]
          # get biomarker and final endpoint responses
          new_probs <- f(res_list[[i]][[j]]$rr)
          draw <- t(stats::rmultinom(res_list[[i]]$alloc_ratio[j-4], 1, new_probs))
          new_resp_bio <- 0
          new_resp_hist <- 0
          for (k in 1:nrow(draw)) {
            if (draw[k,2] == 1) {
              new_resp_bio <- new_resp_bio + 1
            }
            if (draw[k,3] == 1) {
              new_resp_hist <- new_resp_hist + 1
            }
            if (draw[k,4] == 1) {
              new_resp_hist <- new_resp_hist + 1
              new_resp_bio <- new_resp_bio + 1
            }
          }
          res_list[[i]][[j]]$resp_bio <- c(res_list[[i]][[j]]$resp_bio, new_resp_bio)
          res_list[[i]][[j]]$resp_hist <- c(res_list[[i]][[j]]$resp_hist, new_resp_hist)
        }

      } else {

        for (j in 5:8) {
          # get sample sizes
          res_list[[i]][[j]]$n <- c(res_list[[i]][[j]]$n, res_list[[i]]$alloc_ratio[j-4])
          patients_timestamp <- patients_timestamp + res_list[[i]]$alloc_ratio[j-4]
          # get biomarker and final endpoint responses
          new_probs <- f(res_list[[i]][[j]]$rr)
          draw <- t(stats::rmultinom(res_list[[i]]$alloc_ratio[j-4], 1, new_probs))
          new_resp_bio <- 0
          new_resp_hist <- 0
          for (k in 1:nrow(draw)) {
            if (draw[k,2] == 1) {
              new_resp_bio <- new_resp_bio + 1
            }
            if (draw[k,3] == 1) {
              new_resp_hist <- new_resp_hist + 1
            }
            if (draw[k,4] == 1) {
              new_resp_hist <- new_resp_hist + 1
              new_resp_bio <- new_resp_bio + 1
            }
          }
          res_list[[i]][[j]]$resp_bio <- c(res_list[[i]][[j]]$resp_bio, new_resp_bio)
          res_list[[i]][[j]]$resp_hist <- c(res_list[[i]][[j]]$resp_hist, new_resp_hist)
        }
      }
    }


    # For drugs that are not active, add NA
    for (i in cohorts_finished) {
      # in case there was the initial cohort, which would lead to different amounts of arms
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        # Get new patients and responders for every cohorts
        for (j in 5:7) {
          # get sample sizes
          res_list[[i]][[j]]$n <- c(res_list[[i]][[j]]$n, NA)
          # get biomarker and final endpoint responses
          res_list[[i]][[j]]$resp_bio <- c(res_list[[i]][[j]]$resp_bio, NA)
          res_list[[i]][[j]]$resp_hist <- c(res_list[[i]][[j]]$resp_hist, NA)
        }
      } else {
        # Get new patients and responders for every cohorts
        for (j in 5:8) {
          # get sample sizes
          res_list[[i]][[j]]$n <- c(res_list[[i]][[j]]$n, NA)
          # get biomarker and final endpoint responses
          res_list[[i]][[j]]$resp_bio <- c(res_list[[i]][[j]]$resp_bio, NA)
          res_list[[i]][[j]]$resp_hist <- c(res_list[[i]][[j]]$resp_hist, NA)
        }
      }
    }

    # Add patients since last cohort was added
    last_cohort_time <- last_cohort_time + patients_timestamp


    ##### Safety Stopping #####

    # check whether any cohort should stop for safety
    for (i in cohorts_left) {
      # compute 1- probability that no safety stopping
      safety <- stats::rbinom(1, 1, 1 - ((1 - safety_prob) ^ patients_timestamp))
      if (safety) {
        if (res_list[[i]]$decision[1] == "none")  {res_list[[i]]$decision[1] <- "STOP_SAFETY"}
        res_list[[i]]$decision[2] <- "STOP_SAFETY"
        res_list[[i]]$final_n <- sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
        res_list[[i]]$sup_final <- FALSE
        res_list[[i]]$final_n_cohort <- total_n(res_list[[i]])
        if(is.null(res_list[[i]]$interim_n)) {res_list[[i]]$interim_n <- NA}
        if(is.null(res_list[[i]]$interim_n_cohort)) {res_list[[i]]$interim_n_cohort <- NA}
        if(is.null(res_list[[i]]$sup_interim)) {res_list[[i]]$sup_interim <- NA}
        if(is.null(res_list[[i]]$fut_interim)) {res_list[[i]]$fut_interim <- NA}
      }
    }


    if (sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE) > (cohorts_max * (n_fin + 3 * cohorts_max))) {
      stop("Total Sample Size is greater than should be possible with settings")
    }

    ##### Interim Analyses #####

    # check whether any interim analyses should be conducted based on sample size and no safety event

    ind_int <- intersect(
      which(sapply(res_list, function(x) total_n(x)) >= sapply(res_list, function(x) x$n_thresh[1])),
      which(sapply(res_list, function(x) x$decision[1]) %in% c("none"))
    )

    # if interim analyses should be conducted, do so and change n_thresh
    if (length(ind_int) > 0) {
      for (i in ind_int) {
        res_list <- make_decision_trial(res_list, which_cohort = i, interim = TRUE, sharing_type = sharing_type, ...)
        res_list[[i]]$interim_n <- sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
        res_list[[i]]$interim_n_cohort <- sum(total_n(res_list[[i]]), na.rm = TRUE)

        # What happens at successful interim
        if (res_list[[i]]$decision[1] == "GO_SUP") {
          res_list[[i]]$decision[2] <- "GO_SUP"
          res_list[[i]]$n_thresh <- c(Inf, Inf)
          if (first_success == -1) {
            first_success <- length(res_list[[i]][[7]]$n)
          }
        }

        # What happens at unsuccessful interim
        if (res_list[[i]]$decision[1] == "STOP_FUT") {
          res_list[[i]]$decision[2] <- "STOP_FUT"
          res_list[[i]]$n_thresh <- c(Inf, Inf)
        }

        # What happens if Promising (so far nothing)
        if (res_list[[i]]$decision[1] == "PROMISING") {
          res_list[[i]]$n_thresh <- c(Inf, n_fin)
        }

        # What happens if no decision taken
        if (res_list[[i]]$decision[1] == "CONTINUE") {
          res_list[[i]]$n_thresh <- c(Inf, n_fin)
        }

      }
    }

    ##### Final Analyses #####

    # check whether any final analyses should be conducted

    ind_fin <- intersect(
      which(sapply(res_list, function(x) total_n(x)) >= sapply(res_list, function(x) x$n_thresh[2])),
      which(sapply(res_list, function(x) x$decision[2]) %in% c("none", "PROMISING", "CONTINUE"))
    )

    # if final analyses should be conducted, do so and change final decision
    if (length(ind_fin) > 0) {
      for (i in ind_fin) {
        res_list <- make_decision_trial(res_list, which_cohort = i, interim = FALSE, sharing_type = sharing_type, ...)
        res_list[[i]]$final_n <- sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
        res_list[[i]]$final_n_cohort <- total_n(res_list[[i]])
        res_list[[i]]$n_thresh <- c(Inf, Inf)
      }

      if (res_list[[i]]$decision[2] == "GO_SUP") {
        if (first_success == -1) {
          first_success <- length(res_list[[i]][[7]]$n)
        }
      }
    }

    ##### Wrapup and cohort add #####

    # check whether any trial stopping rules reached
    if (is_sr_reached(res_list, sr_drugs_pos, sr_pats, cohorts_max * (n_fin + 3 * cohorts_max))) {
      trial_stop <- 1
      ind_stop_sup <- which(sapply(res_list, function(x) x$decision[2]) %in% c("none", "PROMISING", "CONTINUE"))
      for (j in ind_stop_sup) {
        res_list[[j]]$decision[2] <- "STOP_SR"
        res_list[[j]]$final_n <- sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
        res_list[[j]]$final_n_cohort <- total_n(res_list[[j]])
        res_list[[j]]$sup_final <- FALSE

        # If there was no interim, make sure these values still exist so plot function works
        if (is.null(res_list[[j]]$interim_n)) {
          res_list[[j]]$interim_n <- NA
          res_list[[j]]$interim_n_cohort <- NA
          res_list[[j]]$sup_interim <- NA
          res_list[[j]]$fut_interim <- NA
        }
      }


      # If there was no interim, make sure these values still exist so plot function works
      ind_stop_prior <- which(!sapply(res_list, function(x) x$decision[2]) %in% c("none", "PROMISING", "CONTINUE"))
      for (j in ind_stop_prior) {
        if (is.null(res_list[[j]]$interim_n)) {
          res_list[[j]]$interim_n <- NA
          res_list[[j]]$interim_n_cohort <- NA
          res_list[[j]]$sup_interim <- NA
          res_list[[j]]$fut_interim <- NA
        }
      }
    }

    # check whether further cohorts should be included
    if (first_success == -1 | !sr_first_pos) {
      if (length(res_list) < cohorts_max) {
        if (!trial_stop) {
          if(!is.null(cohort_random)) {
            if (last_cohort_time >= cohort_offset) {
              # 1- probability that no new cohort after x patients
              prob_new <- 1 - ((1 - cohort_random) ^ patients_timestamp)
              new_cohort <- stats::rbinom(1, 1, prob_new)
              if (new_cohort) {
                if (trial_struc == "all_plac") {
                  plac <- TRUE
                }

                if (trial_struc == "no_plac") {
                  plac <- FALSE
                }

                if (trial_struc == "stop_post_mono") {
                  if (mono_suc == 0) {
                    # A bit more complicated for mono comparisons. Firstly, check only cohorts which already have a final decision.
                    # Check only those which do not have a "STOP_SAFETY" decision.
                    # Of those cohorts, check either sup_final_list or, (in case was efficacious at interim), sup_interim_list
                    # Comparisons of Mono vs Placebo are in third and fourth column. All those have to be TRUE.
                    if (any(!(sapply(res_list, function(x) x$decision[2]) %in% c("none", "STOP_SAFETY")))) {
                      # Get indices for those cohorts that have a final decision that is not STOP_SAFETY
                      cohorts_outcome <- which(!(sapply(res_list, function(x) (x$decision[2] %in% c("none", "STOP_SAFETY")))))
                      for (i in cohorts_outcome) {
                        if (!is.null(res_list[[i]]$sup_final_list)) {
                          mat_result <- res_list[[i]]$sup_final_list[[1]]
                        } else {
                          mat_result <- res_list[[i]]$sup_interim_list[[1]]
                        }
                        success_mono <- apply(mat_result, MARGIN = 2, function(x) all(x, na.rm = TRUE))[3:4]
                        if (any(success_mono)) {
                          mono_suc <- 1
                        }
                      }
                      if (mono_suc) {
                        plac <- FALSE
                      } else {
                        plac <- TRUE
                      }
                    } else {
                      plac <- TRUE
                    }
                    # if already one successful, no need to check anymore
                  } else {
                    plac <- FALSE
                  }
                }

                if (trial_struc == "stop_post_back") {
                  if (back_suc == 0) {
                    # A bit more complicated for mono comparisons. Firstly, check only cohorts which already have a final decision.
                    # Check only those which do not have a "STOP_SAFETY" decision.
                    # Of those cohorts, check either sup_final_list or, (in case was efficacious at interim), sup_interim_list
                    # Comparisons of Back vs Placebo is in third column. All those have to be TRUE.
                    if (any(!(sapply(res_list, function(x) x$decision[2]) %in% c("none", "STOP_SAFETY")))) {
                      # Get indices for those cohorts that have a final decision that is not STOP_SAFETY
                      cohorts_outcome <- which(!(sapply(res_list, function(x) (x$decision[2] %in% c("none", "STOP_SAFETY")))))
                      for (i in cohorts_outcome) {
                        if (!is.null(res_list[[i]]$sup_final_list)) {
                          mat_result <- res_list[[i]]$sup_final_list[[1]]
                        } else {
                          mat_result <- res_list[[i]]$sup_interim_list[[1]]
                        }
                        success_back <- apply(mat_result, MARGIN = 2, function(x) all(x, na.rm = TRUE))[3]
                        if (success_back) {
                          back_suc <- 1
                        }
                      }
                      if (back_suc) {
                        plac <- FALSE
                      } else {
                        plac <- TRUE
                      }
                    } else {
                      plac <- TRUE
                    }
                    # if already one successful, no need to check anymore
                  } else {
                    plac <- FALSE
                  }
                }

                # only add new cohort if no combo so far successful
                comb_suc <- any(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "GO")

                if (!comb_suc) {
                  res_list <- create_cohort_new(res_list, plac, n_int, n_fin, sharing_type,
                                                rr_comb_vec, rr_mono_vec, rr_back_vec, rr_plac_vec)
                }
              }
            }
          }
        }
      }
    }

    # If all cohorts are stopped, stop trial
    if (!any(sapply(res_list, function(x) x$decision[2]) %in% c("none", "PROMISING", "CONTINUE"))) {
      trial_stop <- 1
    }

    # Get total Sample Size until now
    Total_N_Vector <- c(Total_N_Vector, sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE))

  }

  ##### Return Values #####

  # Add certain values for cohorts that stopped at interim
  for (i in 1:length(res_list)) {
    if (is.null(res_list[[i]]$final_n)) {
      res_list[[i]]$final_n <- NA
      res_list[[i]]$final_n_cohort <- NA
      res_list[[i]]$sup_final <- NA
      res_list[[i]]$fut_final <- NA
    }
    if (is.null(res_list[[i]]$interim_n) & is.null(res_list[[i]]$final_n)) {
      res_list[[i]]$interim_n <- NA
      res_list[[i]]$interim_n_cohort <- NA
      res_list[[i]]$sup_interim <- NA
      res_list[[i]]$fut_interim <- NA
      res_list[[i]]$final_n <- sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)
      res_list[[i]]$final_n_cohort <- NA
      res_list[[i]]$sup_final <- NA
      res_list[[i]]$fut_final <- NA
    }
  }

  # Make sure plot function always works
  if (n_int == n_fin) {
    for (i in 1:length(res_list)) {
      res_list[[i]]$interim_n <- NA
      res_list[[i]]$interim_n_cohort <- NA
      res_list[[i]]$sup_interim <- NA
      res_list[[i]]$fut_interim <- NA
    }
  }


  # Define truth via:
  # a) Risk Difference
  # a1) Combo > Mono/Back + delta1
  # a2) Mono/Back > Plac + delta2
  # b) Risk Ratio
  # b1) Combo/Mono & Combo/Back > delta1
  # b2) Mono/Plac & Back/Plac > delta2
  # c) Odds Ratio
  # c1) oddsCombo/oddsMono & oddsCombo/oddsBack > delta1
  # c2) oddsMono/oddsPlac & oddsBack/oddsPlac > delta2

  truth <- rep(NA, length(res_list))

  if (target_rr[3] == 1) {
    for (i in 1:length(res_list)) {
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        truth[i] <-
          (res_list[[i]][["Comb"]]$rr > res_list[[i]][["Mono"]]$rr + target_rr[1]) &
          (res_list[[i]][["Comb"]]$rr > res_list[[i]][["Back"]]$rr + target_rr[1])
      } else {
        truth[i] <-
          (res_list[[i]][["Comb"]]$rr > res_list[[i]][["Mono"]]$rr + target_rr[1]) &
          (res_list[[i]][["Comb"]]$rr > res_list[[i]][["Back"]]$rr + target_rr[1]) &
          (res_list[[i]][["Mono"]]$rr > res_list[[i]][["Plac"]]$rr + target_rr[2]) &
          (res_list[[i]][["Back"]]$rr > res_list[[i]][["Plac"]]$rr + target_rr[2])
      }
    }
  }

  if (target_rr[3] == 2) {
    for (i in 1:length(res_list)) {
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        truth[i] <-
          (res_list[[i]][["Comb"]]$rr / res_list[[i]][["Mono"]]$rr > target_rr[1]) &
          (res_list[[i]][["Comb"]]$rr / res_list[[i]][["Back"]]$rr > target_rr[1])
      } else {
        truth[i] <-
          (res_list[[i]][["Comb"]]$rr / res_list[[i]][["Mono"]]$rr > target_rr[1]) &
          (res_list[[i]][["Comb"]]$rr / res_list[[i]][["Back"]]$rr > target_rr[1]) &
          (res_list[[i]][["Mono"]]$rr / res_list[[i]][["Plac"]]$rr > target_rr[2]) &
          (res_list[[i]][["Back"]]$rr / res_list[[i]][["Plac"]]$rr > target_rr[2])
      }
    }
  }

  if (target_rr[3] == 3) {
    odds <- function(x) {x/(1-x)}
    for (i in 1:length(res_list)) {
      if (length(res_list[[i]]$alloc_ratio) == 3) {
        truth[i] <-
          (odds(res_list[[i]][["Comb"]]$rr) / odds(res_list[[i]][["Mono"]]$rr) > target_rr[1]) &
          (odds(res_list[[i]][["Comb"]]$rr) / odds(res_list[[i]][["Back"]]$rr) > target_rr[1])
      } else {
        truth[i] <-
          (odds(res_list[[i]][["Comb"]]$rr) / odds(res_list[[i]][["Mono"]]$rr) > target_rr[1]) &
          (odds(res_list[[i]][["Comb"]]$rr) / odds(res_list[[i]][["Back"]]$rr) > target_rr[1]) &
          (odds(res_list[[i]][["Mono"]]$rr) / odds(res_list[[i]][["Plac"]]$rr) > target_rr[2]) &
          (odds(res_list[[i]][["Back"]]$rr) / odds(res_list[[i]][["Plac"]]$rr) > target_rr[2])
      }
    }
  }

  # Get final experimental response rates
  rr_comb_final <- sapply(res_list, function(x) x$Comb$rr)
  rr_mono_final <- sapply(res_list, function(x) x$Mono$rr)
  rr_back_final <- sapply(res_list, function(x) x$Back$rr)
  rr_plac_final <- unlist(sapply(res_list, function(x) x$Plac$rr))

  # Number of patients on arms that are superior to placebo
  # If all cohorts have placebo, easy, just compare response rates and choose only certain patients.
  # What to do if no placebo or not all cohorts placebo? For cohorts that have placebo, do regular comparison. For cohorts, that do not:
  # If only one value, no problem. If multiple values, use expected value.

  # Theoretical response rates (only for number of cohorts)

  # R> c[p < 0]
  # numeric(0)
  # R> c[p < 0] < p[p<0]
  # logical(0)
  # R> which(c[p < 0] < p[p<0])
  # integer(0)

  c <- rr_comb_vec[1:length(res_list)]
  m <- rr_mono_vec[1:length(res_list)]
  b <- rr_back_vec[1:length(res_list)]
  p <- rr_plac_vec[1:length(res_list)]
  p_real <- unlist(sapply(res_list, function(x) x$Plac$rr))

  comb_pats <- sapply(res_list, function(x) x$Comb$n)
  if (length(comb_pats) == 1) {comb_pats <- as.matrix(comb_pats)}
  comb_pat_sup_th <- sum(comb_pats[, which(c > p)], na.rm = TRUE)
  comb_pat_sup_real <- sum(comb_pats[, which(c[1:length(p_real)] > p_real)], na.rm = TRUE)

  mono_pats <- sapply(res_list, function(x) x$Mono$n)
  if (length(mono_pats) == 1) {mono_pats <- as.matrix(mono_pats)}
  mono_pat_sup_th <- sum(mono_pats[, which(m > p)], na.rm = TRUE)
  mono_pat_sup_real <- sum(mono_pats[, which(m[1:length(p_real)] > p_real)], na.rm = TRUE)

  back_pats <- sapply(res_list, function(x) x$Back$n)
  if (length(back_pats) == 1) {back_pats <- as.matrix(back_pats)}
  back_pat_sup_th <- sum(back_pats[, which(b > p)], na.rm = TRUE)
  back_pat_sup_real <- sum(back_pats[, which(b[1:length(p_real)] > p_real)], na.rm = TRUE)

  perc_n_sup_th <- (comb_pat_sup_th + mono_pat_sup_th + back_pat_sup_th) / sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE)

  if (trial_struc != "no_plac") {
    could_have_been_randomised <-
      sum(sapply(res_list[1:length(p_real)], function(x) x$Plac$n), na.rm = TRUE) +
      sum(sapply(res_list[1:length(p_real)], function(x) x$Comb$n), na.rm = TRUE) +
      sum(sapply(res_list[1:length(p_real)], function(x) x$Mono$n), na.rm = TRUE) +
      sum(sapply(res_list[1:length(p_real)], function(x) x$Back$n), na.rm = TRUE)
    perc_n_sup_real <- (comb_pat_sup_real + mono_pat_sup_real + back_pat_sup_real) / could_have_been_randomised
  } else {
    could_have_been_randomised <- 0
    perc_n_sup_real <- NA
  }

  # Average number of treatments, subjects and subjects on control to first success
  # Get time stamp of first success and then compute numbers

  if (first_success > 0) {

  comb_pats_to_first_success <- sum(sapply(res_list, function(x) sum(x$Comb$n[1:first_success], na.rm = TRUE)), na.rm = TRUE)
  mono_pats_to_first_success <- sum(sapply(res_list, function(x) sum(x$Mono$n[1:first_success], na.rm = TRUE)), na.rm = TRUE)
  back_pats_to_first_success <- sum(sapply(res_list, function(x) sum(x$Back$n[1:first_success], na.rm = TRUE)), na.rm = TRUE)
  plac_pats_to_first_success <- sum(sapply(res_list, function(x) sum(x$Plac$n[1:first_success], na.rm = TRUE)), na.rm = TRUE)

  df <- sapply(res_list, function(x) x$Comb$n[1:first_success])
  # get number of columns that are not exclusivly NAs
  if (!is.null(ncol(df))) {
    cohorts_to_first_success <- ncol(df) - length(which(colSums(df, na.rm = TRUE) == 0))
  } else {
    cohorts_to_first_success <- 1
  }

  } else {

    comb_pats_to_first_success <- NA
    mono_pats_to_first_success <- NA
    back_pats_to_first_success <- NA
    plac_pats_to_first_success <- NA
    cohorts_to_first_success   <- NA
  }

  # Check which decisions were correct positives, false positives etc.
  cp <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "GO" &  truth)
  fp <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "GO" & !truth)
  cn <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "ST" & !truth)
  fn <- sum(substring(sapply(res_list, function(x) x$decision[2]), 1, 2) == "ST" &  truth)

  # Prepare return list
  ret <- list(
    Decision               = sapply(res_list, function(x) x$decision),
    RR_Comb                = rr_comb_final,
    RR_Mono                = rr_mono_final,
    RR_Back                = rr_back_final,
    RR_Plac                = rr_plac_final,
    RR_Target              = target_rr,
    N_Cohorts              = length(res_list),
    N_Cohorts_First_Suc    = cohorts_to_first_success,
    Total_N_Vector         = Total_N_Vector,
    Final_N_Cohort         = final_n_cohort(res_list),
    Total_N                = sum(sapply(res_list, function(x) total_n(x)), na.rm = TRUE),
    Total_N_First_Suc      = comb_pats_to_first_success + back_pats_to_first_success + mono_pats_to_first_success + plac_pats_to_first_success,
    Perc_N_Sup_Plac_Th     = perc_n_sup_th,
    Perc_N_Sup_Plac_Real   = perc_n_sup_real,
    Total_N_Comb           = sum(sapply(res_list, function(x) sum(x$Comb$n, na.rm = TRUE)), na.rm = TRUE),
    Total_N_Mono           = sum(sapply(res_list, function(x) sum(x$Mono$n, na.rm = TRUE)), na.rm = TRUE),
    Total_N_Back           = sum(sapply(res_list, function(x) sum(x$Back$n, na.rm = TRUE)), na.rm = TRUE),
    Total_N_Plac           = sum(sapply(res_list, function(x) sum(x$Plac$n, na.rm = TRUE)), na.rm = TRUE),
    Total_N_Plac_First_Suc = plac_pats_to_first_success,
    Total_N_Plac_Pool      = could_have_been_randomised,
    Successes_Hist         = sum(sapply(res_list, function(x) total_rh(x)), na.rm = TRUE),
    Successes_Hist_Comb    = sum(sapply(res_list, function(x) sum(x$Comb$resp_hist, na.rm = TRUE)), na.rm = TRUE),
    Successes_Hist_Mono    = sum(sapply(res_list, function(x) sum(x$Mono$resp_hist, na.rm = TRUE)), na.rm = TRUE),
    Successes_Hist_Back    = sum(sapply(res_list, function(x) sum(x$Back$resp_hist, na.rm = TRUE)), na.rm = TRUE),
    Successes_Hist_Plac    = sum(sapply(res_list, function(x) sum(x$Plac$resp_hist, na.rm = TRUE)), na.rm = TRUE),
    Successes_Bio          = sum(sapply(res_list, function(x) total_rb(x)), na.rm = TRUE),
    Successes_Bio_Comb     = sum(sapply(res_list, function(x) sum(x$Comb$resp_bio, na.rm = TRUE)), na.rm = TRUE),
    Successes_Bio_Mono     = sum(sapply(res_list, function(x) sum(x$Mono$resp_bio, na.rm = TRUE)), na.rm = TRUE),
    Successes_Bio_Back     = sum(sapply(res_list, function(x) sum(x$Back$resp_bio, na.rm = TRUE)), na.rm = TRUE),
    Successes_Bio_Plac     = sum(sapply(res_list, function(x) sum(x$Plac$resp_bio, na.rm = TRUE)), na.rm = TRUE),
    TP                     = cp,
    FP                     = fp,
    TN                     = cn,
    FN                     = fn,
    any_P                  = as.numeric((cp + fp) > 0)
    )

  if (stage_data) {
    ret <- list(Trial_Overview = ret, Stage_Data = res_list)
  }

  return(ret)

}
