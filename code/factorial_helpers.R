prepare_factorial_effects <- function(
  data,
  value_column,
  metric,
  improvement = c("lower", "higher"),
  scale = c("difference", "log_ratio"),
  require_complete_four = TRUE
) {
  improvement <- match.arg(improvement)
  scale <- match.arg(scale)

  wide <- data %>%
    select(room_id, receiver_id, centre_frequency_hz, condition_id, value = all_of(value_column)) %>%
    mutate(condition_id = as.character(condition_id)) %>%
    pivot_wider(names_from = condition_id, values_from = value)

  if (require_complete_four) {
    wide <- wide %>%
      filter(if_all(all_of(CONDITION_LEVELS), is.finite))
  }

  if (scale == "log_ratio") {
    wide <- wide %>% filter(if_all(all_of(CONDITION_LEVELS), ~ is.finite(.x) & .x > 0))
    contrast <- function(first, second) log(first / second)
  } else {
    contrast <- function(first, second) first - second
  }

  if (improvement == "lower") {
    curtain_no_carpet <- contrast(wide$open_no_carpet, wide$closed_no_carpet)
    curtain_carpet <- contrast(wide$open_carpet, wide$closed_carpet)
    carpet_open <- contrast(wide$open_no_carpet, wide$open_carpet)
    carpet_closed <- contrast(wide$closed_no_carpet, wide$closed_carpet)
  } else {
    curtain_no_carpet <- contrast(wide$closed_no_carpet, wide$open_no_carpet)
    curtain_carpet <- contrast(wide$closed_carpet, wide$open_carpet)
    carpet_open <- contrast(wide$open_carpet, wide$open_no_carpet)
    carpet_closed <- contrast(wide$closed_carpet, wide$closed_no_carpet)
  }

  identifying <- wide %>%
    transmute(
      room_id = as.numeric(room_id),
      receiver_id,
      receiver_block = paste0("room", room_id, "_receiver", receiver_id),
      centre_frequency_hz
    )

  bind_rows(
    identifying %>% mutate(effect_family = "curtain", stratum = "no_carpet", effect_value = curtain_no_carpet),
    identifying %>% mutate(effect_family = "curtain", stratum = "carpet", effect_value = curtain_carpet),
    identifying %>% mutate(effect_family = "carpet", stratum = "open", effect_value = carpet_open),
    identifying %>% mutate(effect_family = "carpet", stratum = "closed", effect_value = carpet_closed),
    identifying %>% mutate(
      effect_family = "interaction",
      stratum = "difference_in_curtain_effects",
      effect_value = curtain_carpet - curtain_no_carpet
    )
  ) %>%
    mutate(metric = metric, scale = scale, .before = effect_family)
}

factorial_support <- function(effects) {
  effects %>%
    distinct(metric, scale, room_id, receiver_id, receiver_block, centre_frequency_hz) %>%
    count(metric, scale, room_id, centre_frequency_hz, name = "n_complete_four_receivers") %>%
    arrange(metric, scale, room_id, centre_frequency_hz)
}

estimate_factorial <- function(effects, level = c("global", "band", "room", "room_band")) {
  level <- match.arg(level)

  cell_means <- effects %>%
    group_by(metric, scale, effect_family, room_id, centre_frequency_hz, stratum) %>%
    summarise(
      n_receivers = n_distinct(receiver_block),
      cell_mean = mean(effect_value),
      .groups = "drop"
    ) %>%
    group_by(metric, scale, effect_family, room_id, centre_frequency_hz) %>%
    summarise(estimate = mean(cell_mean), .groups = "drop")

  if (level == "room_band") return(cell_means)

  if (level == "band") {
    return(cell_means %>%
      group_by(metric, scale, effect_family, centre_frequency_hz) %>%
      summarise(estimate = mean(estimate), .groups = "drop"))
  }

  room_means <- cell_means %>%
    group_by(metric, scale, effect_family, room_id) %>%
    summarise(estimate = mean(estimate), .groups = "drop")

  if (level == "room") return(room_means)

  room_means %>%
    group_by(metric, scale, effect_family) %>%
    summarise(estimate = mean(estimate), .groups = "drop")
}

resample_receiver_profiles <- function(effects) {
  blocks <- effects %>% distinct(room_id, receiver_id)
  sampled <- blocks %>%
    group_by(room_id) %>%
    reframe(
      receiver_id = sample(receiver_id, size = n(), replace = TRUE),
      bootstrap_receiver_id = seq_len(n())
    )

  effects %>%
    inner_join(sampled, by = c("room_id", "receiver_id"), relationship = "many-to-many") %>%
    mutate(
      receiver_id = bootstrap_receiver_id,
      receiver_block = paste0("room", room_id, "_bootstrap", bootstrap_receiver_id)
    ) %>%
    select(-bootstrap_receiver_id)
}

bootstrap_factorial <- function(
  effects,
  level = c("global", "band", "room", "room_band"),
  R = BOOT_N,
  seed = BOOT_SEED
) {
  level <- match.arg(level)
  set.seed(seed)
  draws <- vector("list", R)
  for (b in seq_len(R)) {
    draws[[b]] <- estimate_factorial(resample_receiver_profiles(effects), level = level) %>%
      mutate(bootstrap_id = b, .before = 1)
  }
  bind_rows(draws)
}

jackknife_factorial <- function(effects, level = c("global", "band", "room", "room_band")) {
  level <- match.arg(level)
  blocks <- effects %>% distinct(room_id, receiver_id) %>% arrange(room_id, receiver_id)
  draws <- vector("list", nrow(blocks))
  for (i in seq_len(nrow(blocks))) {
    omitted_room <- blocks$room_id[[i]]
    omitted_receiver <- blocks$receiver_id[[i]]
    draws[[i]] <- effects %>%
      filter(!(room_id == omitted_room & receiver_id == omitted_receiver)) %>%
      estimate_factorial(level = level) %>%
      mutate(
        jackknife_id = i,
        omitted_room_id = omitted_room,
        omitted_receiver_id = omitted_receiver,
        .before = 1
      )
  }
  bind_rows(draws)
}

bca_limits <- function(theta, bootstrap_values, jackknife_values, conf = 0.95) {
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
  jackknife_values <- jackknife_values[is.finite(jackknife_values)]
  alpha <- (1 - conf) / 2
  percentile <- quantile(bootstrap_values, c(alpha, 1 - alpha), names = FALSE, type = 7)

  proportion_below <- (
    sum(bootstrap_values < theta) + 0.5 * sum(bootstrap_values == theta)
  ) / length(bootstrap_values)
  lower_bound <- 1 / (2 * length(bootstrap_values))
  proportion_below <- min(max(proportion_below, lower_bound), 1 - lower_bound)
  z0 <- qnorm(proportion_below)

  jackknife_mean <- mean(jackknife_values)
  deviations <- jackknife_mean - jackknife_values
  denominator <- 6 * sum(deviations^2)^(3 / 2)
  acceleration <- if (denominator == 0) 0 else sum(deviations^3) / denominator

  z_alpha <- qnorm(c(alpha, 1 - alpha))
  adjusted_alpha <- pnorm(z0 + (z0 + z_alpha) / (1 - acceleration * (z0 + z_alpha)))
  adjusted_alpha <- pmin(pmax(adjusted_alpha, 0), 1)
  bca <- quantile(bootstrap_values, adjusted_alpha, names = FALSE, type = 7)

  c(
    percentile_low = percentile[[1]],
    percentile_high = percentile[[2]],
    bca_low = bca[[1]],
    bca_high = bca[[2]],
    bca_bias_correction = z0,
    bca_acceleration = acceleration,
    bootstrap_n = length(bootstrap_values),
    jackknife_n = length(jackknife_values)
  )
}

factorial_intervals <- function(observed, bootstrap, jackknife, keys) {
  rows <- vector("list", nrow(observed))
  for (i in seq_len(nrow(observed))) {
    key_match_boot <- rep(TRUE, nrow(bootstrap))
    key_match_jack <- rep(TRUE, nrow(jackknife))
    for (key in keys) {
      key_match_boot <- key_match_boot & bootstrap[[key]] == observed[[key]][[i]]
      key_match_jack <- key_match_jack & jackknife[[key]] == observed[[key]][[i]]
    }
    limits <- bca_limits(
      observed$estimate[[i]],
      bootstrap$estimate[key_match_boot],
      jackknife$estimate[key_match_jack]
    )
    rows[[i]] <- bind_cols(observed[i, , drop = FALSE], tibble::as_tibble_row(limits))
  }
  bind_rows(rows)
}

factorial_contributions <- function(
  effects,
  level = c("global", "band", "room", "room_band")
) {
  level <- match.arg(level)

  dimensions <- effects %>%
    group_by(metric, scale, effect_family) %>%
    summarise(
      n_rooms = n_distinct(room_id),
      n_bands = n_distinct(centre_frequency_hz),
      n_strata = n_distinct(stratum),
      .groups = "drop"
    )

  weighted <- effects %>%
    group_by(metric, scale, effect_family, room_id, centre_frequency_hz, stratum) %>%
    mutate(n_receivers_in_cell = n_distinct(receiver_block)) %>%
    ungroup() %>%
    left_join(dimensions, by = c("metric", "scale", "effect_family")) %>%
    mutate(
      room_divisor = if (level %in% c("global", "band")) n_rooms else 1L,
      band_divisor = if (level %in% c("global", "room")) n_bands else 1L,
      weight = 1 / (room_divisor * band_divisor * n_strata * n_receivers_in_cell),
      weighted_effect = weight * effect_value
    )

  grouping <- c("metric", "scale", "effect_family")
  if (level %in% c("band", "room_band")) grouping <- c(grouping, "centre_frequency_hz")
  if (level %in% c("room", "room_band")) grouping <- c(grouping, "room_id")

  weighted %>%
    group_by(across(all_of(c(grouping, "receiver_block")))) %>%
    summarise(contribution = sum(weighted_effect), .groups = "drop")
}

block_signflip <- function(contributions, max_permutations = Inf, seed = BOOT_SEED) {
  contributions <- contributions[is.finite(contributions) & contributions != 0]
  n_blocks <- length(contributions)
  observed <- sum(contributions)
  total_permutations <- 2^n_blocks
  tolerance <- .Machine$double.eps^0.5

  if (total_permutations <= max_permutations) {
    chunk_size <- 65536L
    extreme <- 0
    for (start in seq(0, total_permutations - 1, by = chunk_size)) {
      index <- as.integer(start:min(start + chunk_size - 1, total_permutations - 1))
      statistics <- numeric(length(index))
      for (j in seq_len(n_blocks)) {
        signs <- ifelse(bitwAnd(index, bitwShiftL(1L, j - 1L)) == 0L, -1, 1)
        statistics <- statistics + signs * contributions[[j]]
      }
      extreme <- extreme + sum(abs(statistics) >= abs(observed) - tolerance)
    }
    p_value <- extreme / total_permutations
    enumeration <- "complete"
    n_permutations <- total_permutations
  } else {
    n_permutations <- as.integer(max_permutations)
    set.seed(seed)
    extreme <- 0
    chunk_size <- 10000L
    remaining <- n_permutations
    while (remaining > 0) {
      current <- min(chunk_size, remaining)
      signs <- matrix(sample(c(-1, 1), current * n_blocks, replace = TRUE), nrow = current)
      statistics <- as.vector(signs %*% contributions)
      extreme <- extreme + sum(abs(statistics) >= abs(observed) - tolerance)
      remaining <- remaining - current
    }
    p_value <- (extreme + 1) / (n_permutations + 1)
    enumeration <- "monte_carlo"
  }

  tibble(
    estimate_from_contributions = observed,
    n_blocks = n_blocks,
    n_permutations = n_permutations,
    enumeration = enumeration,
    sign_symmetry_p_two_sided = p_value
  )
}

signflip_factorial <- function(
  contributions,
  keys,
  max_permutations = Inf,
  seed = BOOT_SEED
) {
  groups <- contributions %>%
    distinct(across(all_of(keys))) %>%
    arrange(across(all_of(keys)))
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    keep <- rep(TRUE, nrow(contributions))
    for (key in keys) keep <- keep & contributions[[key]] == groups[[key]][[i]]
    result <- block_signflip(
      contributions$contribution[keep],
      max_permutations = max_permutations,
      seed = seed + i
    )
    rows[[i]] <- bind_cols(groups[i, , drop = FALSE], result)
  }
  bind_rows(rows)
}
