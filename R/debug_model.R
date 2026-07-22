#' Step-by-step Debug Runner
#'
#' Runs each stage of \code{model_run} independently and returns a data frame
#' showing which step passed and which threw an error. Use this to pinpoint
#' exactly where the failure occurs on the server.
#'
#' @return A data frame with columns \code{step} and \code{result}.
#' @export
debug_model <- function() {

  steps  <- character(0)
  results <- character(0)

  .record <- function(name, expr) {
    steps  <<- c(steps, name)
    results <<- c(results, tryCatch({
      force(expr)
      "OK"
    }, error = function(e) paste0("ERROR: ", conditionMessage(e))))
  }

  # 1. heemod available?
  .record("heemod_available", requireNamespace("heemod", quietly = FALSE))

  # 2. define_parameters with bquote
  params_obj <- NULL
  .record("define_parameters", {
    params_obj <<- eval(bquote(heemod::define_parameters(
      p_subrec_epi2_base = 0.25,
      p_sub_epi1    = heemod::rate_to_prob(0.0158, to = 1/4),
      p_subrec_epi2 = heemod::rescale_prob(p = p_subrec_epi2_base, 1/4),
      or_recurrence = 1.16,
      p_subrec_epi3 = heemod::or_to_prob(or = or_recurrence, p = p_subrec_epi2),
      p_remi_drug   = 65 / 190,
      p_remi_cbt    = 65 / 190,
      p_remi_com    = 79 / 159,
      cost_epi_cbt  = 463,
      cost_epi_dru  = 324,
      cost_epi_com  = 502,
      ut_epi_base   = 0.63,
      ut_epi        = ut_epi_base / 4,
      carbon_epi_dru = 104,
      scc           = .(90),
      dr            = .(0.025),
      dru_duration  = .(8L),
      dru_duration_com = .(8L)
    )))
  })

  # 3. define_transition (minimal, just 2 states)
  trans_obj <- NULL
  .record("define_transition", {
    trans_obj <<- heemod::define_transition(
      C, p_remi_drug,
      0, 1,
      state_names = c("episode", "remission")
    )
  })

  # 4. define_state
  state_obj <- NULL
  .record("define_state", {
    state_obj <<- heemod::define_state(
      utility = ut_epi,
      cost    = heemod::discount(cost_epi_dru, dr, period = 4, linear = TRUE),
      carbon  = carbon_epi_dru,
      cost_with_carbon = carbon * scc / 1000 + cost
    )
  })

  # 5. heemod::discount standalone
  .record("discount_function", {
    heemod::discount(100, 0.025, period = 4, linear = TRUE)
  })

  # 6. dispatch_strategy must be tested inside define_state + run_model context
  # (calling it standalone always errors — heemod injects .strategy only during
  # model evaluation). Run a minimal 2-state model to verify it works end-to-end.
  .record("dispatch_strategy_in_context", {
    p <- eval(bquote(heemod::define_parameters(dr = .(0.025), cost_a = 100, cost_b = 200)))
    tr <- heemod::define_transition(C, 0.1, 0, 1, state_names = c("A", "B"))
    s_a <- heemd_s_a <- heemod::define_state(
      cost = heemod::dispatch_strategy(s1 = cost_a, s2 = cost_b),
      util = 0.8
    )
    s_b <- heemod::define_state(cost = 0, util = 0)
    st1 <- heemod::define_strategy("A" = s_a, "B" = s_b, transition = tr)
    st2 <- heemod::define_strategy("A" = s_a, "B" = s_b, transition = tr)
    suppressMessages(heemod::run_model(
      s1 = st1, s2 = st2, parameters = p,
      cycles = 3L, cost = cost, effect = util
    ))
  })

  data.frame(step = steps, result = results, stringsAsFactors = FALSE)
}
