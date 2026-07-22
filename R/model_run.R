#' Run the Depression Carbon Footprint Model
#'
#' Runs a Markov model comparing three depression treatment strategies
#' (cognitive behavioural therapy, pharmacotherapy, combined) on healthcare
#' costs, QALYs, and carbon footprint of care. Both a base cost-effectiveness
#' analysis and a carbon-adjusted analysis are returned.
#'
#' The underlying model is embedded directly in this package. It uses the
#' \pkg{heemod} package for the Markov simulation.
#'
#' @param model_input Named list (single scenario) or data frame (batch of
#'   scenarios), or \code{NULL} to use \code{\link{get_default_input}()}.
#'
#' @section Input fields:
#' \describe{
#'   \item{scc}{Social cost of carbon (USD per tonne CO2e). Default: 90.}
#'   \item{dr}{Annual discount rate. Default: 0.025.}
#'   \item{dru_duration}{Drug treatment duration in model cycles (quarters).
#'     Default: 8.}
#'   \item{dru_duration_com}{Combined therapy drug duration in model cycles.
#'     Defaults to \code{dru_duration} when omitted.}
#'   \item{cycles}{Number of model cycles (quarters). Default: 20.}
#'   \item{seed}{Random seed (for reproducibility). Default: 1.}
#' }
#'
#' @return A data frame with one row per strategy (\code{cbt}, \code{drug},
#'   \code{combined}) and the following columns:
#'   \describe{
#'     \item{strategy}{Treatment strategy name.}
#'     \item{total_cost}{Total discounted healthcare cost (USD).}
#'     \item{total_utility}{Total discounted QALYs.}
#'     \item{icer_base}{Incremental cost per QALY vs. drug (healthcare costs
#'       only). \code{NA} for the reference strategy (drug).}
#'     \item{total_carbon_kgco2e}{Total carbon footprint (kg CO2e), derived
#'       from the difference between carbon-adjusted and base costs.}
#'     \item{total_cost_carbon_adj}{Total discounted cost including the social
#'       cost of carbon (USD).}
#'     \item{icer_carbon_adj}{Incremental cost per QALY vs. drug (carbon-
#'       adjusted). \code{NA} for the reference strategy (drug).}
#'   }
#' @export
model_run <- function(model_input = NULL) {

  if (is.null(model_input)) {
    model_input <- get_default_input()
  }

  # ---- Coerce JSON column-array lists to data frame --------------------------
  # OpenCPU deserialises a JSON object of arrays as a named list, not a
  # data.frame.  Detect this case (all elements are equal-length vectors with
  # length > 1) and convert so the batch path is triggered correctly.
  if (is.list(model_input) && !is.data.frame(model_input)) {
    elem_lengths <- vapply(model_input, length, integer(1L))
    if (length(elem_lengths) > 0L &&
        length(unique(elem_lengths)) == 1L &&
        unique(elem_lengths) > 1L) {
      model_input <- as.data.frame(model_input, stringsAsFactors = FALSE)
    }
  }

  # ---- Batch: loop over rows -------------------------------------------------
  if (is.data.frame(model_input)) {
    results <- lapply(seq_len(nrow(model_input)), function(i) {
      row_result <- model_run(as.list(model_input[i, ]))
      row_result$input_id <- i
      row_result
    })
    return(do.call(rbind, results))
  }

  # ---- Single: named list (wrapped in tryCatch to surface R errors as 200) ----
  if (is.list(model_input)) {
    return(tryCatch(
      .model_run_core(model_input),
      error = function(e) data.frame(
        error   = TRUE,
        message = conditionMessage(e),
        call    = paste(deparse(conditionCall(e)), collapse = " "),
        stringsAsFactors = FALSE
      )
    ))
  }

  stop("model_input must be a named list or a data frame.", call. = FALSE)
}

# Internal worker — separated so tryCatch above can catch any R error and
# return it as a 200 (visible) response instead of an opaque 400.
.model_run_core <- function(model_input) {

    # Extract and coerce parameters (use different variable names so they
    # do not shadow heemod parameter names inside define_parameters())
    scc_val          <- if (!is.null(model_input$scc))
                          as.numeric(model_input$scc) else 90
    dr_val           <- if (!is.null(model_input$dr))
                          as.numeric(model_input$dr) else 0.025
    dru_dur_val      <- if (!is.null(model_input$dru_duration))
                          as.numeric(model_input$dru_duration) else 8
    dru_dur_com_val  <- if (!is.null(model_input$dru_duration_com))
                          as.numeric(model_input$dru_duration_com) else dru_dur_val
    cycles_val       <- if (!is.null(model_input$cycles))
                          as.integer(model_input$cycles) else 20L
    seed_val         <- if (!is.null(model_input$seed))
                          as.integer(model_input$seed) else 1L

    set.seed(seed_val)

    # --------------------------------------------------------------------------
    # Parameters
    # bquote() substitutes local variables as literals so heemod does not need
    # to resolve them from its own evaluation environment.
    # --------------------------------------------------------------------------
    parameters <- eval(bquote(heemod::define_parameters(
      p_subrec_epi2_base = 0.25,
      p_sub_epi1    = heemod::rate_to_prob(0.0158, to = 1/4),
      p_subrec_epi2 = heemod::rescale_prob(p = p_subrec_epi2_base, 1/4),
      or_recurrence = 1.16,
      p_subrec_epi3 = heemod::or_to_prob(or = or_recurrence,     p = p_subrec_epi2),
      p_subrec_epi4 = heemod::or_to_prob(or = or_recurrence^2,   p = p_subrec_epi2),
      p_subrec_epi5 = heemod::or_to_prob(or = or_recurrence^3,   p = p_subrec_epi2),
      p_remi_drug = 65 / 190,
      p_remi_com  = 79 / 159,
      p_remi_cbt  = 65 / 190,
      p_chr_dth_base = 28.6,
      p_sub_dth_base = 19.2,
      p_chr_dth    = heemod::rate_to_prob(p_chr_dth_base, 1/4, 1000),
      p_sub_dth    = heemod::rate_to_prob(p_sub_dth_base, 1/4, 1000),
      p_subrec_dth = heemod::rate_to_prob(p_sub_dth_base, 1/4, 1000),
      p_epi_dth    = heemod::rate_to_prob(p_chr_dth_base, 1/4, 1000),
      cost_epi_cbt = 463,
      cost_epi_dru = 324,
      cost_epi_com = 502,
      cost_chr_cbt_base = 612,
      cost_chr_cbt = cost_chr_cbt_base / 4,
      cost_chr_dru_base = 423,
      cost_chr_dru = cost_chr_dru_base / 4,
      cost_chr_com_base = 672,
      cost_chr_com = cost_chr_com_base / 4,
      ut_epi_base = 0.63,
      ut_epi      = ut_epi_base / 4,
      ut_sub_base = 0.86,
      ut_sub      = ut_sub_base / 4,
      ut_chr_base = 0.8,
      ut_chr      = ut_chr_base / 4,
      carbon_chr_cbt_base = 338,
      carbon_chr_cbt = carbon_chr_cbt_base / 4,
      carbon_chr_dru_base = 319,
      carbon_chr_dru = carbon_chr_dru_base / 4,
      carbon_chr_com_base = 373,
      carbon_chr_com = carbon_chr_com_base / 4,
      carbon_epi_cbt_base = 177,
      carbon_epi_cbt = carbon_epi_cbt_base / 4,
      carbon_epi_dru_base = 104,
      carbon_epi_dru = carbon_epi_dru_base / 4,
      carbon_epi_com_base = 225,
      carbon_epi_com = carbon_epi_com_base / 4,
      # User-overridable parameters — injected as literals via bquote
      scc          = .(scc_val),
      dr           = .(dr_val),
      dru_duration     = .(dru_dur_val),
      dru_duration_com = .(dru_dur_com_val)
    )))

    # --------------------------------------------------------------------------
    # Transition matrices
    # --------------------------------------------------------------------------
    state_names <- c(
      "subthreshold",
      "subthreshold recovery after 1st episode",
      "subthreshold recovery after 2nd episode",
      "subthreshold recovery after 3rd episode",
      "subthreshold recovery after 4th episode",
      "1st episode", "2nd episode", "3rd episode",
      "4th episode", "5th episode",
      "chronic state", "death"
    )

    transition_dru <- heemod::define_transition(
      0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth,
      0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth,
      0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth,
      0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth,
      0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth,
      0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth,
      0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth,
      0, 0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, p_epi_dth,
      0, 0, 0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
      state_names = state_names
    )

    transition_cbt <- heemod::define_transition(
      0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth,
      0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth,
      0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth,
      0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth,
      0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth,
      0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth,
      0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth,
      0, 0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, p_epi_dth,
      0, 0, 0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
      state_names = state_names
    )

    transition_com <- heemod::define_transition(
      0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth,
      0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth,
      0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth,
      0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth,
      0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth,
      0, p_remi_com, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth,
      0, 0, p_remi_com, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth,
      0, 0, 0, p_remi_com, 0, 0, 0, C, 0, 0, 0, p_epi_dth,
      0, 0, 0, 0, p_remi_com, 0, 0, 0, C, 0, 0, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
      state_names = state_names
    )

    # --------------------------------------------------------------------------
    # States
    # --------------------------------------------------------------------------
    sub_state <- heemod::define_state(
      utility = ut_sub,
      cost = heemod::dispatch_strategy(
        cbt      = 0,
        drug     = heemod::discount(
          ifelse(state_time <= dru_duration, cost_epi_dru, 0),
          dr, period = 4, linear = TRUE),
        combined = heemod::discount(
          ifelse(state_time <= dru_duration_com, cost_epi_dru, 0),
          dr, period = 4, linear = TRUE)
      ),
      carbon = heemod::dispatch_strategy(
        cbt      = 0,
        drug     = ifelse(state_time <= dru_duration,     carbon_epi_dru, 0),
        combined = ifelse(state_time <= dru_duration_com, carbon_epi_dru, 0)
      ),
      cost_with_carbon = carbon * scc / 1000 + cost
    )

    epi_state <- heemod::define_state(
      utility = ut_epi,
      cost = heemod::dispatch_strategy(
        cbt      = heemod::discount(cost_epi_cbt, dr, period = 4, linear = TRUE),
        drug     = heemod::discount(cost_epi_dru, dr, period = 4, linear = TRUE),
        combined = heemod::discount(cost_epi_com, dr, period = 4, linear = TRUE)
      ),
      carbon = heemod::dispatch_strategy(
        cbt      = carbon_epi_cbt,
        drug     = carbon_epi_dru,
        combined = carbon_epi_com
      ),
      cost_with_carbon = carbon * scc / 1000 + cost
    )

    chr_state <- heemod::define_state(
      utility = ut_chr,
      cost = heemod::dispatch_strategy(
        cbt      = heemod::discount(cost_chr_cbt, dr, period = 4, linear = TRUE),
        drug     = heemod::discount(cost_chr_dru, dr, period = 4, linear = TRUE),
        combined = heemod::discount(cost_chr_com, dr, period = 4, linear = TRUE)
      ),
      carbon = heemod::dispatch_strategy(
        cbt      = carbon_chr_cbt,
        drug     = carbon_chr_dru,
        combined = carbon_chr_com
      ),
      cost_with_carbon = carbon * scc / 1000 + cost
    )

    death_state <- heemod::define_state(
      utility          = 0,
      cost             = 0,
      carbon           = 0,
      cost_with_carbon = 0
    )

    # --------------------------------------------------------------------------
    # Strategies
    # --------------------------------------------------------------------------
    .make_strategy <- function(transition) {
      heemod::define_strategy(
        "subthreshold"                              = sub_state,
        "subthreshold recovery after 1st episode"  = sub_state,
        "subthreshold recovery after 2nd episode"  = sub_state,
        "subthreshold recovery after 3rd episode"  = sub_state,
        "subthreshold recovery after 4th episode"  = sub_state,
        "1st episode"   = epi_state,
        "2nd episode"   = epi_state,
        "3rd episode"   = epi_state,
        "4th episode"   = epi_state,
        "5th episode"   = epi_state,
        "chronic state" = chr_state,
        "death"         = death_state,
        transition = transition
      )
    }

    strats_cbt <- .make_strategy(transition_cbt)
    strats_dru <- .make_strategy(transition_dru)
    strats_com <- .make_strategy(transition_com)

    # --------------------------------------------------------------------------
    # Run models (suppress output; two runs — base cost and carbon-adjusted cost)
    # --------------------------------------------------------------------------
    res_base <- suppressMessages(suppressWarnings(
      heemod::run_model(
        cbt      = strats_cbt,
        drug     = strats_dru,
        combined = strats_com,
        parameters       = parameters,
        cycles           = cycles_val,
        cost             = cost,
        effect           = utility,
        central_strategy = "drug",
        state_time_limit = cycles_val
      )
    ))

    res_carbon <- suppressMessages(suppressWarnings(
      heemod::run_model(
        cbt      = strats_cbt,
        drug     = strats_dru,
        combined = strats_com,
        parameters       = parameters,
        cycles           = cycles_val,
        cost             = cost_with_carbon,
        effect           = utility,
        central_strategy = "drug",
        state_time_limit = cycles_val
      )
    ))

    # --------------------------------------------------------------------------
    # Extract results from summary tables
    # --------------------------------------------------------------------------
    .parse_summary <- function(res) {
      s  <- summary(res)
      df <- as.data.frame(s$res_comp)
      # Normalise strategy column name
      strat_col <- grep("strategy", names(df), value = TRUE, ignore.case = TRUE)
      if (length(strat_col) == 0L) strat_col <- names(df)[1L]
      names(df)[names(df) == strat_col[1L]] <- "strategy"
      df
    }

    base_df   <- .parse_summary(res_base)
    carbon_df <- .parse_summary(res_carbon)

    # Locate cost and ICER columns (names match the argument passed to run_model)
    .find_col <- function(df, patterns) {
      hits <- unlist(lapply(patterns, function(p)
        grep(p, names(df), value = TRUE, ignore.case = TRUE)))
      hits <- hits[hits != "strategy"]
      if (length(hits) == 0L) return(NA_character_)
      hits[1L]
    }

    base_cost_col   <- .find_col(base_df,   c("^cost$"))
    base_util_col   <- .find_col(base_df,   c("utility", "effect", "qaly"))
    base_icer_col   <- .find_col(base_df,   c("icer"))
    carbon_cost_col <- .find_col(carbon_df, c("cost_with_carbon", "cost"))
    carbon_icer_col <- .find_col(carbon_df, c("icer"))

    # Build result data frame
    result <- data.frame(
      strategy              = base_df$strategy,
      total_cost            = base_df[[base_cost_col]],
      total_utility         = base_df[[base_util_col]],
      icer_base             = base_df[[base_icer_col]],
      total_cost_carbon_adj = carbon_df[[carbon_cost_col]],
      icer_carbon_adj       = carbon_df[[carbon_icer_col]],
      stringsAsFactors = FALSE
    )

    # Derive total carbon from the cost difference:
    # cost_with_carbon = cost + carbon_kgco2e * scc / 1000
    # => carbon_kgco2e = (cost_with_carbon - cost) * 1000 / scc
    result$total_carbon_kgco2e <- ifelse(
      scc_val > 0,
      (result$total_cost_carbon_adj - result$total_cost) * 1000 / scc_val,
      NA_real_
    )

    # Reorder columns
    result <- result[, c(
      "strategy",
      "total_cost", "total_utility", "icer_base",
      "total_carbon_kgco2e",
      "total_cost_carbon_adj", "icer_carbon_adj"
    )]

    rownames(result) <- NULL
    return(result)
}
