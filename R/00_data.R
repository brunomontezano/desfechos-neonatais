# Armazenar caminho dos dados
raw_path <- "data/raw/banco_hospitais_limpo_18_05_23.sav"

# Carregar dados brutos
dados_raw <- haven::read_sav(raw_path,
                             user_na = FALSE)

# Visualizar dados brutos
glimpse(dados_raw)

# Criar objeto com dicionário de variáveis
dicionario <- dados_raw |>
  janitor::clean_names() |>
  purrr::map(\(x) attributes(x)$label) |>
  purrr::simplify() |>
  tibble::enframe("variavel", "rotulo")

# Exportar dicionário de variáveis em csv
dicionario |>
  readr::write_csv(file = "data/processed/dicionario_variaveis.csv")

# Limpar os dados
dados_limpos <- dados_raw |>
  # Limpar nomes de variáveis para snake_case
  janitor::clean_names() |>
  # Transformar labels nos níveis dos fatores
  haven::as_factor() |>
  # Remover valores inválidos nas variáveis numéricas e categóricas
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              function(x) {
                                if (is.numeric(x)) {
                                    dplyr::case_when(x %in% c(88, 99, 43182, 43157, 43712, 43189, 9999, 999, 888, 43276, 43278, 43779, 43746, 43683, 43682) ~ NA,
                                                   .default = x)
                                } else {
                                  dplyr::case_when(x %in% c("88.0", "88", "", "99", "99.0", "10", "2", "4", "9", "17", "44") ~ NA,
                                                   .default = x) |> as.factor() |> droplevels()
                                }
                              }),
                # Criar fatores com texto nas variáveis dicotômicas abaixo
                dplyr::across(c(inf_anem, us_cort),
                              \(x) dplyr::case_when(x == 1 ~ "sim",
                                                    x == 0 ~ "não") |>
                                as.factor()),
                sexarc_id = dplyr::na_if(sexarc_id, 160),
                apgar_5 = dplyr::if_else(
                  apgar_5 > 10 | apgar_5 < 0, NA,
                  apgar_5
                ),
                num_ciga = dplyr::if_else(
                  num_ciga > 999, NA, num_ciga
                ),
                #id_gest = dplyr::if_else(
                #  id_gest > 60, NA, id_gest
                #),
                pes_peso = dplyr::if_else(
                  pes_peso < 100, NA, pes_peso
                ),
                dplyr::across(c(hospital, numero),
                              \(x) as.character(x)),
                rac_decl = as.factor(dplyr::case_when(
                  rac_decl == "branca" ~ "branca",
                  rac_decl %in% c("negra", "amarela", "parda", "indigena") ~ "não-branca"
                )),
                escol_mat = as.factor(dplyr::case_when(
                  escol_mat %in% c("EF incompleto", "analfabeta") ~ "EF incompleto",
                  escol_mat %in% c("Especialização incompleta", "Especialização completa",
                                   "Mestrado incompleto", "Mestrado completo", "Doutordo completo",
                                   "Doutorado incompleto", "PosDoc completo") ~ "ES completo",
                  escol_mat %in% c("Tecnico Prof incompleto", "Técnico Prof incompleto") ~ "EM completo",
                  .default = escol_mat
                ))) |>
  dplyr::select(-ql_beb, # Qual bebida?
                -freq_beb, # Frequência da bebida
                -id_gest_avamama, # Idade gestacional
                -id_gest_odont, # Idade gestacional da consulta odonto
                -tto_dia, # Tratamento DMG
                -tip_pat, # Tipo de patologia da mãe
                -val_rend_ind, # Renda individual (codificação complicada)
                -ig_anem, # Qual idade gestacional (codificação)
                -fez_tto, # Tratamento sífilis (apenas instâncias positivas)
                -ql_drog, # Qual droga (baixa frequência)
                -freq_drog, # Frequência drogas (baixa frequência)
                -ss_tto, # 22 sim/9 não (baixa frequência)
                -rend_fam) # Renda familiar (codificação complicada)

# Exportar dados limpos para arquivo rds
readr::write_rds(x = dados_limpos, file = "data/processed/dados_limpos.rds")
