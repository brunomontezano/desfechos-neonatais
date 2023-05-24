# Ler dados processados
dados_limpos <- readr::read_rds("data/processed/dados_limpos.rds")

# Peso ao nascer vs. idade gestacional
dados_limpos |>
  dplyr::mutate(id_gest = id_gest / 7) |>
  ggplot2::ggplot(ggplot2::aes(x = id_gest,
                               y = log10(pes_peso))) +
  ggplot2::geom_point(alpha = 0.15,
                      size = 2) +
  ggplot2::geom_smooth(method = "lm",
                       se = FALSE) +
  ggplot2::theme_light(18, "Charter") +
  ggplot2::labs(x = "Idade gestacional (em semanas)",
                y = "log10(Peso ao nascer)") +
  ggpubr::stat_regline_equation(size = 7, family = "Charter",
                                output.type = "expression") +
  ggplot2::annotate("text",
                    x = 45,
                    y = 2.75,
                    label = "Para cada semana de gestação,\nespera-se que o peso ao nascer\naumente em 8,4%.",
                    size = 5,
                    family = "Charter")

# Exportar gráfico do peso ao nascer vs. idade gestacional
#ggplot2::ggsave(filename = "output/figures/peso_idade.pdf",
#                device = cairo_pdf)

# Simular crescimento do bebê
purrr::map_vec(.x = 1:40, # Semana 1 até semana 40
               .f = \(semanas) 10 ^ (log10(158.5) + 0.035 * semanas)) |>
  tibble::as_tibble_col("peso") |>
  tibble::rowid_to_column("semana")
