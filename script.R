# Load packages
my_packages <- c("igraph", "readxl", "openxlsx", "tidyr", "dplyr", "plm","broom", "knitr","fixest")


## Install each package if not installed
for(package_name in my_packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

# Read KSHAP network data
## Extract variable names as characters from the second row of the file
varnames <- read_excel(
  "K_NETWORK_unlocked (1).xlsx",
  col_names = FALSE,
  range = "A2:J2"
) %>% 
  as.character()

## Read the file starting from the 3rd row with column names extracted above
kshap_all_wave_network <- read_excel(
  "K_NETWORK_unlocked (1).xlsx",
  skip = 2,
  col_names = varnames
)

## Subset rows where 'wave' equals 1 using dplyr's filter() function
#kshap_w1_network = wave 1 의 네트워크 데이터
kshap_w1_network <- kshap_all_wave_network %>% 
  filter(wave == 1)

#kshap_w3_network = wave 3 의 네트워크 데이터
kshap_w3_network <- kshap_all_wave_network |> 
  filter(wave ==3)


# Read KSHAP KOSSDA data
## Read the file
kshap_w1 <- read_excel(
  'KSHAP_W1_KOSSDA.xlsx',
  # col_names = TRUE
)


kshap_w3 <- read_excel(
  'kshap_w3_kossda.xlsx',
  col_names = TRUE
)


# Section: Network Data Manipulation -wave1 &wave3

## Select variables of interest
kshap_close_wave1 <- kshap_w1 %>% 
  dplyr::select(rdbid, a161, a162, a163, a164, a165, a166, a167)

kshap_close_wave3 <- kshap_w3 %>% 
  dplyr::select(rdbid,d111,d112,d113,d114,d115,d116,d117)



# Merge KSHAP network data and KOSSDA data using dplyr
kshap_w1_network_close_merged <- kshap_w1_network %>% 
  inner_join(kshap_close_wave1, by = "rdbid")

kshap_w3_network_close_merged <- kshap_w3_network |> 
  inner_join(kshap_close_wave3, by = "rdbid")

## Rename column names for the next step
rename_list <- setNames(paste0("a16", 1:7), paste0("n_close_", 1:7))
rename_list_w3 <- setNames(paste0("d11", 1:7), paste0("n_close_", 1:7))

for (name in names(rename_list)) {
  kshap_w1_network_close_merged <- kshap_w1_network_close_merged %>% 
    rename(!!name := !!sym(rename_list[name]))
}

for (name in names(rename_list_w3)) {
  kshap_w3_network_close_merged <- kshap_w3_network_close_merged %>% 
    rename(!!name := !!sym(rename_list_w3[name]))
}

## Pivot from wide to long with interpersonal closeness attached using tidyr
kshap_w1_network_long <- pivot_longer(
  kshap_w1_network_close_merged,
  cols = c(starts_with("n_rnid_"), starts_with("n_close_")),
  
  ### Split the original column names into two parts
  ### And directly name the number column as "roster"
  names_to = c(".value", "roster"),
  
  ### Regex to split the column names into the value name and a number
  names_pattern = "^(n_[a-z]+_)(\\d+)$"
) %>%
  
  ## Rename column names for convenience
  rename(source = rnid_ego,
         target = n_rnid_,
         weight = n_close_)

kshap_w3_network_long <- pivot_longer(
  kshap_w3_network_close_merged,
  cols = c(starts_with("n_rnid_"), starts_with("n_close_")),
  
  ### Split the original column names into two parts
  ### And directly name the number column as "roster"
  names_to = c(".value", "roster"),
  
  ### Regex to split the column names into the value name and a number
  names_pattern = "^(n_[a-z]+_)(\\d+)$"
) %>%
  ## Rename column names for convenience
  rename(source = rnid_ego,
         target = n_rnid_,
         weight = n_close_)


# Create a nodelist
# Make sure to include both egos' NIDs and alters' NIDs.
kshap_w1_network_node <- union(
  kshap_w1_network_long$source,
  kshap_w1_network_long$target
) %>%
  na.omit() %>%     # remove any NAs
  sort() %>%        # sort the atomic vector
  as.data.frame() %>%
  setNames("Node")

rownames(kshap_w1_network_node) <- seq_len(nrow(kshap_w1_network_node))


kshap_w3_network_node <- union(
  kshap_w3_network_long$source,
  kshap_w3_network_long$target
) %>%
  na.omit() %>%     # remove any NAs
  sort() %>%        # sort the atomic vector
  as.data.frame() %>%
  setNames("Node")

rownames(kshap_w3_network_node) <- seq_len(nrow(kshap_w3_network_node))



# Create an arclist
kshap_w1_network_arc <- kshap_w1_network_long %>% 
  drop_na(target) %>% 
  dplyr::select(source, target, weight)

kshap_w3_network_arc <- kshap_w3_network_long %>% 
  drop_na(target) %>% 
  dplyr::select(source, target, weight)


# Convert nodelist & arclist into igraph object: weighted network
g_w1 <- graph_from_data_frame(
  kshap_w1_network_arc,
  vertices = kshap_w1_network_node,
  directed = TRUE
)
g_w3 <- graph_from_data_frame(
  kshap_w3_network_arc,
  vertices = kshap_w3_network_node,
  directed = TRUE
)
g_w3 <- delete_edges(g_w3, E(g_w3)[is.na(weight)])
E(g_w3)[is.na(weight)]



# Get network metrics
network_metrics_w1 <- data.frame(
  rnid_ego        = as.numeric(V(g_w1)$name),
  degree_w_in     = strength(g_w1, mode = "in"),
  degree_w_out    = strength(g_w1, mode = "out"),
  degree_all      = degree(g_w1, mode = "all"),
  betweenness     = betweenness(g_w1, directed = TRUE, weights = E(g_w1)$weight, normalized = TRUE),
  closeness       = closeness(g_w1, mode = "all", weights = E(g_w1)$weight),
  eigen_centrality= eigen_centrality(g_w1, directed = TRUE, weights = E(g_w1)$weight)$vector,
  kcore_in        = coreness(g_w1, mode = "in"),
  kcore_out       = coreness(g_w1, mode = "out"),
  kcore_all       = coreness(g_w1, mode = "all")
)

network_metrics_w3 <- data.frame(
  rnid_ego        = as.numeric(V(g_w3)$name),
  degree_w_in     = strength(g_w3, mode = "in"),
  degree_w_out    = strength(g_w3, mode = "out"),
  degree_all      = degree(g_w3, mode = "all")
  # betweenness     = betweenness(g_w3, directed = TRUE, weights = E(g_w3)$weight),
  # closeness       = closeness(g_w3, mode = "all", weights = E(g_w3)$weight),
  # eigen_centrality= eigen_centrality(g_w3, directed = TRUE, weights = E(g_w3)$weight)$vector,
  # kcore_in        = coreness(g_w3, mode = "in"),
  # kcore_out       = coreness(g_w3, mode = "out"),
  # kcore_all       = coreness(g_w3, mode = "all"),
  # wave            = 3
)

# Merge network metrics with KOSSDA data
network_w1 <- left_join(kshap_w1_network_close_merged,network_metrics_w1, by = "rnid_ego" )
wave1 <- full_join(kshap_w1, network_w1, by = "rdbid")

network_w3 <- left_join(kshap_w3_network_close_merged,network_metrics_w3, by = "rnid_ego" )
wave3 <- full_join(kshap_w3, network_w3, by = "rdbid")

#degree를 계산했을 때, 노드의 값은 rnid_ego이며, rdbid가아닙니다. 
#네트워크 데이터에서는, rdbid값이 아닌 rnid_ego값으로 서로가 서로의 지목하여 친밀도를 계산하고, 연결됨을 파악함. 
#그러나, survey 데이터에는 rdbid값만이 존재하므로, 해당 노드의 rdbid값을 알수 있도록
#network data(kshap_w1_network)에 degrees_df_w1을 병합합니다.

# WAVE 1 변수 처리

# MMSE 문항 선택
mmse_vars_w1 <- c(
  "a8501", "a8502", "a8503", "a8504", "a8505",
  "a8506", "a8507", "a8508", "a8509", "a8510",
  "a861", "a862", "a863", "a871", "a872",
  "a873", "a874", "a875", "a881", "a882",
  "a883", "a891", "a892", "a90", "a911",
  "a912", "a913", "a92", "a93", "a94"
)
wave1 <- wave1 |>
  mutate(
    wave = 1,
    #discussion network size
    network_size = n_size3,
    # 배우자
    spouse_num = ifelse(a062 %in% c(1, 2), 1, 0),
    spouse_label = factor(spouse_num, levels = c(0, 1), labels = c("No Spouse", "Has Spouse")),    
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    edu = factor(case_when(
      a02 == 1 ~ "No formal education",
      a02 %in% c(2, 3) ~ "Elementary",
      a02 == 4 ~ "Middle School",
      a02 == 5 ~ "High School",
      a02 == 6 ~ "College or higher",
      a02 >= 7 ~ NA_character_,
      TRUE ~ NA_character_
    ),
      levels = c("No formal education", "Elementary", "Middle School", "High School", "College or higher")
    ),
    income = factor(case_when(
      b1091 == 1 ~ "Low",
      b1091 == 2 ~ "Mid-low",
      b1091 == 3 ~ "Mid",
      b1091 %in% c(4, 5) ~ "High",
      TRUE ~ NA_character_
    ),
    levels = c("Low", "Mid-low", "Mid", "High")),
    income_numeric = ifelse(b1091 ==9, NA, b1091),
    married = ifelse(a062 %in% c(1, 2), 1, 0),
    #smoker = ifelse(a47 %in% c(1, 4), 1, 0),
    drinking = factor(
      case_when(
        a48 == 1 ~"Non-Drinker",
        a48 == 2~ "Drinker"
      ), label = c("Non-Drinker","Drinker")
    ),
    #전반적인 건강상태
    self_health = 6-b100, 
    #우울증 수치
    # 1. 역코딩 (문항 a8405, a8410, a8415)
    across(c(a8405, a8410, a8415), ~ 5 - .),
    # 2. 전체 1~4 → 0~3으로 변환
    across(a8401:a8420, ~ . - 1),
    # 3. CES-D 총점 계산
    cesd = rowSums(across(a8401:a8420), na.rm = TRUE),
    across(all_of(mmse_vars_w1), ~ ifelse(. == 9,0, .)),
    mmse = rowSums(across(all_of(mmse_vars_w1)), na.rm = FALSE),
    ) 


# wave3 변수처리

#wave 3: mmse response 응답 문항 선택하기 
mmse_vars_w3 <- c(
  "l01", "l02", "l03", "l04", "l05", "l06", "l07", "l08", "l09", "l10",
  "l11", "l12", "l13", "l14", "l15", "l16", "l17", "l18", "l19", "l20",
  "l21", "l22", "l23", "l24", "l25", "l26", "l27", "l28", "l29", "l30"
)

wave3 <- wave3 %>%
  mutate(
    wave = 3,
    # discussion network size
    network_size = n_size3,
    # 배우자
    spouse_num = ifelse(c3 %in% c(1, 2), 1, 0),
    spouse_label = factor(spouse_num, levels = c(0, 1), labels = c("No Spouse", "Has Spouse")),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    edu = factor(case_when(
      a2 == 1 ~ "No formal education",
      a2 %in% c(2, 3) ~ "Elementary",
      a2 == 4 ~ "Middle School",
      a2 == 5 ~ "High School",
      a2 == 6 ~ "College or higher",
      TRUE ~ NA_character_
    ),levels = c(
      "No formal education", "Elementary", "Middle School", "High School", "College or higher"
    )),
    income = case_when(
      q4 == 1 ~ "Low",
      q4 == 2 ~ "Mid-low",
      q4 == 3 ~ "Mid",
      q4 %in% c(4, 5) ~ "High",
      q4 == 9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    income = factor(income, levels = c("Low", "Mid-low", "Mid", "High")),
    income_numeric = ifelse(q4 ==9, NA, q4),
    smoker = factor(ifelse(f1 %in% c(1, 4), 1, 0),
                  levels = c(0, 1), labels = c("non-smoker", "smoker")),
    drinking = factor(
       case_when(
        g1 %in% c(1,2) ~ "Non-drinker",
        g1 == 3 ~ "Drinker",
        TRUE ~ NA_character_)
      , levels = c(
        "Non-drinker", "Drinker"
      )),
    #전반적인 건강상태
    self_health = ifelse(p1 == 9, NA, p1),
    #CESD
    # 9를 NA로 처리
    across(k1_1:k1_20, ~ifelse(.==9, NA,.)),
    # 역코딩
    k1_5  = ifelse(!is.na(k1_5), 5 - k1_5, NA),
    k1_10 = ifelse(!is.na(k1_10), 5 - k1_10, NA),
    k1_15 = ifelse(!is.na(k1_15), 5 - k1_15, NA),
    # 각 항목을 0~3점으로 변환
    across(k1_1:k1_20, ~ . - 1),
    #ces-d 점수 합산
    cesd = rowSums(across(k1_1:k1_20), na.rm = TRUE),
    # MMSE 점수 계산, 각 항목을 0~3점으로 변환
    across(all_of(mmse_vars_w3), ~ ifelse(. == 9, 0, .)),
    mmse = rowSums(across(all_of(mmse_vars_w3)), na.rm = FALSE),
  )


# PANEL DATA 준비
panel_long <- bind_rows(wave1, wave3) %>%
  select(
    rdbid, wave, rnid_ego,
    network_size,
    degree_w_in, degree_w_out, degree_all,
    betweenness, closeness, eigen_centrality, pagerank,
    kcore_in, kcore_out, kcore_all,
    mmse, mmse_group,
    cesd, depressed,
    sex, age, edu, income, income_numeric,
    married, spouse_label, spouse_num,
    self_health, diabetes, iadl_imp,
    smoker, drinking
  ) |>group_by(rdbid) %>%
  filter(n_distinct(wave) == 2) %>%  # wave 1, 3 모두 있는 경우만
  ungroup() %>%
  arrange(rdbid, wave)

# 패널 데이터로 변환
pdata <- pdata.frame(panel_long, index = c("rdbid","wave"))

# FE (within)
fe <- plm(
  log(mmse + 1) ~ cesd + self_health + spouse_label + age+
    income + degree_w_out,
  data = pdata,
  model = "within",
  index = c("rdbid", "wave")
)
summary(fe)

# RE (random)
re <- plm(
  log(mmse + 1) ~ cesd + self_health + spouse_label + age +
    income + degree_w_out + sex,  # 변하지 않는 변수는 RE에서 가능
  data = pdata,
  model = "random",
  index = c("rdbid", "wave")
)
summary(re)

phtest(fe,re)

# CRE
# 집단 평균(개인별 평균) 변수 추가
pdata_cre <- pdata %>%
  group_by(rdbid) %>%
  mutate(
    mean_cesd = mean(cesd, na.rm = TRUE),
    mean_self_health = mean(self_health, na.rm = TRUE),
    mean_income_numeric = mean(income_numeric, na.rm = TRUE),
    mean_degree_w_out = mean(degree_w_out, na.rn = TRUE),
    mean_spouse_num = mean(spouse_num, na.rm = TRUE)
  ) %>%
  ungroup()

# CRE 모델 (RE + 평균 변수 추가)
# 시간변수(변하는 변수)는 원자료 + 평균
cre <- plm(
  log(mmse + 1) ~ cesd + mean_cesd +
    self_health + mean_self_health +
     income_numeric + mean_income_numeric +
    degree_w_out + mean_degree_w_out +
    spouse_num + mean_spouse_num+
     sex + edu ,
  data = pdata_cre,
  model = "random",
  index = c("rdbid", "wave")
)

summary(cre)

