library(tidyverse)
library(patchwork)

label_alpha = label_bquote(cols = {alpha~"="~.(alpha)})

running_example_data <- function(N) {
  N <- 5e2
  mu <- rep(0, N)
  yhat <- mu
  sigma <- ifelse(1:N <= N / 2, 0.2, 0.05)
  y <- rnorm(N, mean = mu, sd = sigma)
  list(y = y, yhat = yhat)
}

extract_metric <- function(results, metric) {
  unlist(lapply(results, function(result) result$metrics[[metric]]))
}

extract_intervals <- function(fit) {
  tibble(
    lower = fit$intervals[,1],
    upper = fit$intervals[,2],
    pred  = fit$predictions[,1]
  )
}

format_coverage <- scales::percent_format()
format_path_length <- scales::number_format(accuracy = 0.1)
format_mean_width <- scales::number_format(accuracy = 0.01)

aci_theme <- theme(axis.title.y = element_text(size = 8))

basic_theme <- theme(
  axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(), 
  legend.position = "none"
)

simulation_one_plot <- function(results) {
  labels = ggplot2::label_bquote(
    rows = {alpha~"="~.(alpha)}, 
    cols = {psi~"="~theta~"="~.(param)}
  )
  
  plot_coverr <- results %>%
    ggplot(aes(x = method, y = coverage - alpha, color = method)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    facet_grid(alpha ~ param, labeller = labels) +
    labs(
      x = "", 
      y = expression(CovErr(T)), 
      subtitle = "Coverage Error", 
      title = "Simulation study: ARMA errors"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    aci_theme
  
  plot_piwidth <- results %>%
    ggplot(aes(x = method, y = mean_width, color = method)) +
    geom_boxplot() +
    facet_grid(alpha ~ param, scale = "free_y", labeller = labels) +
    labs(x = "", y = expression(MeanWidth(T)), subtitle = "Mean Interval Width") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    aci_theme
  
  plot_pathlength <- results %>%
    ggplot(aes(x = method, y = path_length, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_grid(alpha ~ param, scale = "free_y", labeller = labels) +
    labs(x = "", y = expression(PathLength(T)), subtitle = "Path Length") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  plot_sareg <- results %>%
    ggplot(aes(x = method, y = strongly_adaptive_regret, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_grid(alpha ~ param, scale = "free_y", labeller = labels) +
    labs(x = "", y = expression(SAReg(T, m)), subtitle = "Strongly Adaptive Regret") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  plot_coverr / plot_piwidth / plot_pathlength / plot_sareg & theme(plot.margin = margin(0, 0, 0, 0, "cm"))
}

simulation_one_joint_plot <- function(results) {
  labels = ggplot2::label_bquote(
    rows = {alpha~"="~.(alpha)}, 
    cols = {psi~"="~theta~"="~.(param)}
  )
  
  simulation_study1$results %>% 
    mutate(coverr = coverage - alpha) %>%
    group_by(param, alpha, N, method) %>%
    summarize_at(vars(coverr, mean_width), 
                 list(mean = mean, qlower = ~quantile(., 0.1), qupper = ~quantile(., 0.9))) %>%
    ggplot(aes(x = coverr_mean, y = mean_width_mean, color = method)) +
    geom_segment(aes(x = coverr_qlower, xend = coverr_qupper, y = mean_width_mean, yend = mean_width_mean), alpha = 0.5) +
    geom_segment(aes(x = coverr_mean,   xend = coverr_mean, y = mean_width_qlower, yend = mean_width_qupper), alpha = 0.5) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
    facet_grid(alpha ~ param, scales = "free_y", labeller = labels) +
    labs(x = expression(CovErr(T)), y = expression(MeanWidth(T)), title = "Simulation Study: ARMA errors") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

simulation_two_plot <- function(results) {
  results <- results %>% mutate(
    distribution_shift = ifelse(distribution_shift == 0.5, "Shift", "No shift")
  )
  
  labels = ggplot2::label_bquote(
    rows = .(distribution_shift),
    cols = {alpha~"="~.(alpha)}
  )
  
  plot_coverr <- results %>%
    ggplot(aes(x = method,
               y = coverage - alpha, color = method)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    facet_grid(distribution_shift ~ alpha, labeller = labels) +
    labs(x = "", y = expression(CovErr(T)), subtitle = "Coverage Error", 
         title = "Simulation Study:\nDistribution Shift") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75))) +
    aci_theme
  
  plot_piwidth <- results %>%
    ggplot(aes(x = method,
               y = mean_width, color = method)) +
    geom_boxplot() +
    facet_grid(distribution_shift ~ alpha, labeller = labels, scales = "free_y") +
    labs(x = "", y = expression(MeanWidth(T)), subtitle = "Mean Interval Width") +
    aci_theme +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)))
  
  plot_pathlength <- results %>%
    ggplot(aes(x = method,
               y = path_length, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_grid(distribution_shift ~ alpha, labeller = labels, scales = "free_y") +
    labs(x = "ACI Method", y = expression(PathLength(T)), subtitle = "Path Length") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75))) +
    aci_theme
  
  plot_sareg <- results %>%
    ggplot(aes(x = method,
               y = strongly_adaptive_regret, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_grid(distribution_shift ~ alpha, labeller = labels, scales = "free_y") +
    labs(x = "ACI Method", y = expression(SAReg(T, m)), subtitle = "Strongly Adaptive Regret") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75))) +
    aci_theme
  
  (plot_coverr + plot_piwidth) / (plot_pathlength + plot_sareg)
}

case_study_plot <- function(results) {
  plot_coverr <- results %>%
    ggplot(aes(x = method, y = coverage - alpha, color = method)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    facet_wrap(~alpha, labeller = label_alpha) +
    labs(x = "", y = expression(CovErr(T))) +
    labs(subtitle = "Coverage Error", title = "Case Study Results") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  plot_piwidth <- results %>%
    ggplot(aes(x = method, y = mean_width, color = method)) +
    geom_boxplot() +
    facet_wrap(~alpha, labeller = label_alpha, scales = "free_y") +
    labs(x = "", y = expression(MeanWidth(T))) +
    labs(subtitle = "Interval Width") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  plot_pathlength <- results %>%
    ggplot(aes(x = method, y = path_length, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_wrap(~alpha, labeller = label_alpha) +
    labs(x = "ACI Method", y = expression(PathLength(T))) +
    labs(subtitle = "Path Length") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  plot_sareg <- results %>%
    ggplot(aes(x = method, y = strongly_adaptive_regret, color = method)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_wrap(~alpha, labeller = label_alpha) +
    labs(x = "ACI Method", y = expression(SAReg(T, m))) +
    labs(subtitle = "Strongly Adaptive Regret") +
    aci_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  
  (plot_coverr / plot_piwidth / plot_pathlength / plot_sareg)
}

simulation_two_joint_plot <- function(results) {
  labels = ggplot2::label_bquote(
    rows = {alpha~"="~.(alpha)}, 
    cols = {psi~"="~theta~"="~.(param)}
  )
  
  pdata <- results %>%
    mutate(coverr = coverage - alpha,
           distribution_shift = factor(distribution_shift, levels = c(0, 0.5), labels = c("Without shift", "With shift"))) %>%
    rename(Method = method, `Distribution Shift` = distribution_shift) %>%
    group_by(`Distribution Shift`, alpha, N, Method) %>%
    summarize_at(vars(coverr, mean_width, path_length), 
                 list(mean = mean, qlower = ~quantile(., 0.1), qupper = ~quantile(., 0.9)))
  
  p1 <- pdata %>%
    ggplot(aes(x = coverr_mean, y = mean_width_mean, color = Method)) +
    geom_segment(aes(x = coverr_qlower, xend = coverr_qupper, y = mean_width_mean, yend = mean_width_mean), alpha = 0.5) +
    geom_segment(aes(x = coverr_mean,   xend = coverr_mean, y = mean_width_qlower, yend = mean_width_qupper), alpha = 0.5) +
    geom_point(aes(shape = `Distribution Shift`), size = 2.5) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
    facet_wrap(~alpha,  labeller = label_bquote(alpha~"="~.(alpha))) +
    labs(x = expression(CovErr(T)), y = expression(MeanWidth(T)), title = "Simulation Study: Distribution Shift") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- pdata %>%
    ggplot(aes(x = coverr_mean, y = path_length_mean, color = Method)) +
    geom_segment(aes(x = coverr_qlower, xend = coverr_qupper, y = path_length_mean, yend = path_length_mean), alpha = 0.5) +
    geom_segment(aes(x = coverr_mean,   xend = coverr_mean, y = path_length_qlower, yend = path_length_qupper), alpha = 0.5) +
    geom_point(aes(shape = `Distribution Shift`), size = 2.5) +
    scale_y_log10() +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
    facet_wrap(~alpha,  labeller = label_bquote(alpha~"="~.(alpha))) +
    labs(x = expression(CovErr(T)), y = expression(PathLength(T))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  (p1 / p2)
}


run_simulation_study1 <- function(data, setup, estimate_model, fit, workers = 8) {
  cache <- here::here("./simulation_studies/simulation_study1.rds")
  if(file.exists(cache)) return(read_rds(cache))
  
  plan(multisession, workers = workers)
  with_progress({
    p <- progressor(steps = nrow(data))
    data <- data %>%
      mutate(data = pmap(list(index, psi, xi, N), simulate),
             preds = future_map(data, estimate_model, p = p, 
                                .options = furrr_options(seed = TRUE)))
    
    setup <- expand_grid(
      data,
      setup
    )
    
    p <- progressor(steps = nrow(setup))
    simulation_study <- setup %>%
      mutate(
        fit = future_pmap(list(data, preds, method, alpha), fit, p = p, 
                          .options = furrr_options(seed = TRUE)),
        metrics = future_map(fit, metrics, .options = furrr_options(seed = TRUE))
      ) %>%
      select(-data)
  })
  
  results <- list(
    example_fits = simulation_study %>% 
      filter(param == 0.8, index == 1, alpha == 0.9),
    results =  simulation_study %>%
      mutate(metrics = map(metrics, as_tibble)) %>%
      select(-fit) %>%
      unnest(c(metrics)) 
  )
  
  write_rds(results, cache)
  
  results
}

run_simulation_study2 <- function(setup, fit, workers = 8) {
  cache <- here::here("./simulation_studies/simulation_study2.rds")
  if(file.exists(cache)) return(read_rds(cache))
  
  plan(multisession, workers = workers)
  
  with_progress({
    p <- progressor(steps = nrow(setup))
    
    simulation_study <- setup %>%
      mutate(
        fit = future_pmap(list(data, method, alpha), fit, p = p, 
                          .options = furrr_options(seed = TRUE)),
        metrics = map(fit, metrics)
      )
  })
  
  results <- list(
    example_fits = simulation_study %>% 
      filter(distribution_shift == 0.5, index == 1, alpha == 0.9),
    results = simulation_study %>%
      select(-data) %>%
      mutate(metrics = map(metrics, as_tibble)) %>%
      select(-fit) %>%
      unnest(c(metrics)) 
  )
  
  write_rds(results, cache)
  
  results
}