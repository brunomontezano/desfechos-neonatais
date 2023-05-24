# Carregar ambiente da predição do peso
#load("cache/predicao_peso.RData")

# Ler dados limpos
dados_limpos <- readr::read_rds("data/processed/dados_limpos.rds")

# Remover demais desfechos da base de dados
dados_peso <- dados_limpos |>
  dplyr::select(-apgar_5, -id_gest, -rece_nasc) |>
  dplyr::filter(!is.na(pes_peso)) |>
  haven::zap_label() |>
  as.data.frame()

# Setar seed
set.seed(1)

# Divisão de dados em treino e teste
divisao <- rsample::initial_split(data = dados_peso,
                                  prop = 0.75,
                                  strata = "pes_peso")

# Criar objetos para cada subconjunto
treino <- rsample::training(divisao)
teste <- rsample::testing(divisao)

# Criar validação cruzada
# Setar seed
set.seed(1)

# Criar 10-fold repetido dez vezes
validacao <- rsample::vfold_cv(treino,
                               v = 10,
                               repeats = 10,
                               strata = "pes_peso")

# Criar receita de pré-processamento dos dados
receita <- treino |>
  recipes::recipe(pes_peso ~ .) |>
  recipes::update_role(numero, new_role = "id") |>
  recipes::update_role(hospital, new_role = "hospital") |>
  recipes::step_filter_missing(recipes::all_predictors(),
                               threshold = 0.1) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  recipes::step_impute_mean(recipes::all_numeric_predictors()) |>
  recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
  recipes::step_corr(recipes::all_numeric_predictors(),
                     threshold = 0.9) |>
  recipes::step_dummy(recipes::all_nominal_predictors())

# Base de treino processada
treino_preprocessado <- receita |>
  recipes::prep() |>
  recipes::bake(new_data = NULL)

# Especificar o modelo
# Regressão logística regularizada
modelo <- parsnip::linear_reg(
  mixture = tune::tune(),
  penalty = tune::tune(),
  mode = "regression",
  engine = "glmnet"
)

# Criar objeto do workflow com receita e modelo
fluxo <- workflows::workflow(
  preprocessor = receita,
  spec = modelo
)

# Criar grade de hiperparâmetros com lambda e alfa
grade_hiperparametros <- dials::grid_regular(
  dials::penalty(range = c(-5, 5)),
  dials::mixture(range = c(0, 1)),
  levels = 10
)

# Setar seed novamente
set.seed(1)

# Criar objeto com reamostras da validação cruzada
reamostras <- tune::tune_grid(
  object = fluxo,
  resamples = validacao,
  grid = grade_hiperparametros,
  metrics = yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::rsq,
    yardstick::mape
  ),
  control = tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    save_pred = TRUE,
    save_workflow = TRUE
  )
)

# Visualizar validação cruzada
reamostras |>
  ggplot2::autoplot() +
  ggplot2::theme_light(18, "IBM Plex Sans")

reamostras |>
  tune::show_best("rmse")

fluxo_final <- fluxo |>
  tune::finalize_workflow(reamostras |>
                            tune::select_best(metric = "rmse"))

ajuste_final <- fluxo_final |>
  tune::last_fit(split = divisao)

ajuste_final |>
  tune::collect_metrics()

ajuste_final |>
  tune::collect_predictions() |>
  ggplot2::ggplot(ggplot2::aes(x = .pred,
                               y = pes_peso)) +
  ggplot2::geom_point(alpha = 0.7,
                      size = 2) +
  #ggplot2::geom_smooth(method = "lm") +
  ggplot2::geom_abline(intercept = 0,
                       slope = 1,
                       color = "red",
                       linewidth = 1.5,
                       alpha = 0.5) +
  ggplot2::theme_light(18, "Charter") +
  ggplot2::labs(x = "Peso predito",
                y = "Peso ao nascer")

ajuste_final |>
  hardhat::extract_fit_engine() |>
  vip::vip(geom = "point",
           aesthetics = list(color = "steelblue")) +
  ggplot2::theme_light(18, "Charter")

ajuste_final |>
  hardhat::extract_fit_engine() |>
  coef(s = 46.4)

#save.image(file = "cache/predicao_peso.RData")
#load("cache/predicao_peso.RData")
