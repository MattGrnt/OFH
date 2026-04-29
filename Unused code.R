library(minpack.lm)

c1w2_expected_bookings <- w2_sends$mailing_size[1] * w2_sends$conversion_rate[1]
day_offset <- 12

clinic1_w2_total <- total_bookings |>
  filter(clinic == "clinic 1", day > day_offset) |>
  mutate(
    day_rebased    = day - day_offset,
    cumul_rebased  = cumul_bookings - min(cumul_bookings)
  )

lambda_start <- clinic1_w2_total |>
  filter(cumul_rebased >= 0.63 * max(cumul_rebased)) |>
  slice(1) |>
  pull(day_rebased)

fit <- nlsLM(cumul_rebased ~ Asym * (1 - exp(-(day_rebased / lambda)^k)),
             data = clinic1_w2_total,
             start = list(Asym = max(clinic1_w2_total$cumul_rebased),
                          lambda = lambda_start,
                          k = 1.5))

clinic_w2_params <- tidy(fit) |>
  select(term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  mutate(predicted_final = Asym + c1w2_expected_bookings,          # estimated total bookings
         day_80          = lambda * (-log(0.2))^(1/k) + day_offset,
         pct_complete    = max(clinic1_w2_total$cumul_bookings) / Asym)



simulate_bookings <- function(
    n_mailed,
    # referral_rate_range, # c(min, max)
    n_days = 28,
    decay_rate = 0.15,   # higher = faster dropoff
    seed = 42
) {
  set.seed(seed)
  
  # Draw a referral rate from the range (uniform prior)
  referral_rate <- 0.015
  
  # Total number of people who will eventually book
  n_bookers <- rbinom(1, n_mailed, referral_rate)
  
  # Probability of booking on each day — exponential decay, normalised
  days <- 1:n_days
  day_probs <- exp(-decay_rate * days)
  day_probs <- day_probs / sum(day_probs)
  
  # Sample a booking day for each booker
  booking_days <- sample(days, n_bookers, replace = TRUE, prob = day_probs)
  
  # Aggregate to daily counts
  tibble(day = days) |>
    left_join(
      tibble(day = booking_days) |> count(day),
      by = "day"
    ) |>
    mutate(
      n = replace_na(n, 0),
      referral_rate = referral_rate,
      n_bookers = n_bookers
    )
}

# Example
daily_bookings <- simulate_bookings(n_mailed = 5000#, referral_rate_range = c(0.05, 0.15)
)



sims <- map(1:500, \(i) simulate_bookings(5000, 
                                          seed = i)) |>
  list_rbind(names_to = "sim_id")

# Summarise across sims
prediction <- sims |>
  summarise(
    median = median(n),
    lo = quantile(n, 0.1),
    hi = quantile(n, 0.9),
    .by = day
  )

# Starting parameters
booking_opens       <- as.Date("2026-05-04")
clinic_opens        <- as.Date("2026-06-15")
clinic_closes       <- as.Date("2026-07-12")

latest_booking_date <- as.Date("2026-07-11")
n_waves             <- 5

weeks <- tibble(
  week            = 1:4,
  capacity        = c(500, 500, 500, 500),
  bookings        = 0,
  remaining       = capacity
)

# wave 1 sizing
conversion_min <- 0.025
conversion_max <- 0.04
conversion_mid <- (conversion_min + conversion_max) / 2 

W1_target_pct <- 0.4
total_capacity   <- sum(weeks$remaining)
target_bookings  <- round(total_capacity * W1_target_pct)

wave1_scenarios <- tibble(
  scenario = c("low", "mid", "high"),
  p_book   = c(conversion_min, conversion_mid, conversion_max),
  wave1_n  = ceiling(target_bookings / p_book)
)

# 
alpha_weeks    <- c(1, 1, 1, 1)
weibull_k      <- 0.7
weibull_lambda <- 3.0
