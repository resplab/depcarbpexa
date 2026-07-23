# depcarbpexa

A [ModelsCloud](https://modelscloud.resp.core.ubc.ca)-compatible R package embedding a heemod-based Markov model that compares three depression treatment strategies — cognitive behavioural therapy (CBT), pharmacotherapy, and combined therapy — on healthcare costs, quality-adjusted life years (QALYs), and carbon footprint of care.

```
modelscloud (R client)
  └─ pexaclient (HTTP)
       └─ PexaCloud / OpenCPU
            └─ depcarbpexa          ← this package
                 └─ heemod (embedded Markov model)
```

## Installation

```r
# Install from GitHub
remotes::install_github("resplab/depcarbpexa")
```

## Quick start via ModelsCloud client

```r
library(modelscloud)

m <- connect_to_model("depcarbpexa")

# Retrieve example inputs
inputs <- get_sample_input(m)

# Run the model
results <- model_run(m, inputs[1, ])
```

## Direct usage

### Single scenario

```r
library(depcarbpexa)

result <- model_run(list(
  scc              = 90,
  dr               = 0.025,
  dru_duration     = 8,
  dru_duration_com = 8,
  cycles           = 20L,
  seed             = 1L
))

#   strategy  total_cost total_utility icer_base total_carbon_kgco2e total_cost_carbon_adj icer_carbon_adj
# 1      cbt    ...         ...          ...          ...                  ...                ...
# 2     drug    ...         ...           NA          ...                  ...                 NA
# 3 combined    ...         ...          ...          ...                  ...                ...
```

Pass `NULL` (or omit the argument) to use the defaults:

```r
result <- model_run()
```

### Batch (multiple scenarios)

Supply a data frame; each row is run independently and results are stacked with an `input_id` column:

```r
inputs <- get_sample_input(n = 3)
results <- model_run(inputs)
```

## Optional parameters

| Parameter | Description | Default |
|---|---|---|
| `scc` | Social cost of carbon (USD per tonne CO₂e). Higher values penalise carbon-intensive strategies more in `icer_carbon_adj`. | 90 |
| `dru_duration_com` | Drug treatment duration in the combined arm (quarters). When omitted, defaults to `dru_duration`. | same as `dru_duration` |
| `seed` | Random seed for reproducibility. | 1 |

## Core functions

| Function | Description |
|---|---|
| `model_run(model_input)` | Run the model. Returns a data frame with one row per strategy. |
| `get_sample_input(n)` | Return `n` example scenarios as a data frame (1–5). |
| `get_default_input()` | Return a named list of default parameter values. |
| `version()` | Return the package name and deployed version string. |
| `echo(model_input)` | Echo input back with class, length, and JSON — for debugging deserialisation. |
| `debug_model()` | Run each internal model stage independently; return step-by-step pass/fail. |

## Input reference

### Required fields

| Field | Type | Range | Description |
|---|---|---|---|
| `scc` | numeric | ≥ 0 | Social cost of carbon (USD/tonne CO₂e). Use 0 to ignore carbon monetisation. |
| `dr` | numeric | 0–1 | Annual discount rate applied to costs and utilities. |
| `dru_duration` | numeric | ≥ 1 | Number of model cycles (quarters) that pharmacotherapy is active in the drug arm. |
| `cycles` | integer | ≥ 1 | Total number of model cycles (each cycle = 1 quarter). |

### Optional fields

| Field | Type | Default | Description |
|---|---|---|---|
| `dru_duration_com` | numeric | = `dru_duration` | Drug duration in the combined arm (quarters). |
| `seed` | integer | 1 | Random seed. |

## Output

One row per strategy (`cbt`, `drug`, `combined`). `drug` is the reference strategy; ICER columns are `NA` for it.

| Column | Type | Description |
|---|---|---|
| `strategy` | character | Treatment strategy: `cbt`, `drug`, or `combined`. |
| `total_cost` | numeric | Total discounted healthcare cost (USD), base analysis. |
| `total_utility` | numeric | Total discounted QALYs. |
| `icer_base` | numeric | Incremental cost per QALY vs. drug (healthcare costs only). `NA` for the reference. |
| `total_carbon_kgco2e` | numeric | Total carbon footprint of care (kg CO₂e), derived from `(cost_with_carbon − cost) × 1000 / scc`. |
| `total_cost_carbon_adj` | numeric | Total discounted cost including the social cost of carbon (USD). |
| `icer_carbon_adj` | numeric | Incremental cost per QALY vs. drug (carbon-adjusted). `NA` for the reference. |

## Related packages

| Package | Role |
|---|---|
| [heemod](https://cran.r-project.org/package=heemod) | Markov health economic model engine (embedded). |
| [modelscloud](https://github.com/resplab/modelscloud) | R client for calling models hosted on ModelsCloud/PexaCloud. |
| [pexaclient](https://github.com/resplab/pexaclient) | HTTP communication layer used by `modelscloud`. |

## License

GPL-3
