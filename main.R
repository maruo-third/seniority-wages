library(tidyverse)
theme_set(theme_grey(base_size = 12, base_family = "HiraKakuPro-W3"))

# 消費者物価指数2015
d.cpi <- read_csv("./data/cpi.csv") %>% rename(cpi_2015 = cpi)

# 賃金構造基本統計調査
csv_files <- paste0("./data/wages/", dir("./data/wages/"))
for(j in 1:length(csv_files)){
  d.temp <- read_csv(csv_files[j]) %>%
    rename_all(function(x){str_replace_all(x, "_基本$|\\(.+\\)$|階級[0-9]$|（.+）$", "")}) %>%
    rename_at(vars(starts_with("年齢")), ~ "年齢階級")
  if(j==1){
    d.wage <- d.temp
  }else{
    d.wage <- d.wage %>% union_all(d.temp)
  }
  rm(d.temp)
}

d.wage <- d.wage %>%
  mutate(
    wage = `きまって支給する現金給与額【千円】`*12 + `年間賞与その他特別給与額【千円】`,
    `年齢階級` = case_when(
      `年齢階級` == "２０〜２４歳" ~ "20〜24歳",
      `年齢階級` == "２５〜２９歳" ~ "25〜29歳",
      `年齢階級` == "３０〜３４歳" ~ "30〜34歳",
      `年齢階級` == "３５〜３９歳" ~ "35〜39歳",
      `年齢階級` == "４０〜４４歳" ~ "40〜44歳",
      `年齢階級` == "４５〜４９歳" ~ "45〜49歳",
      `年齢階級` == "５０〜５４歳" ~ "50〜54歳",
      `年齢階級` == "５５〜５９歳" ~ "55〜59歳",
      T ~ `年齢階級`
    ),
    year = as.numeric(str_extract(`時間軸`,"[0-9]{4}"))
  ) %>%
  mutate(birth_year = year - as.numeric(str_extract(`年齢階級`, "[0-9]{2}")) -4) %>%
  select(year, birth_year, wage)

# 実質賃金に変換してグラフ化
d.wage.cpi <- d.wage %>%
  inner_join(d.cpi, by = "year") %>% 
  mutate(
    real_wage = wage/cpi_2015,
    age = year - birth_year -4
  )

d.wage.cpi %>%
  filter(birth_year >= 1975) %>%
  mutate(age = year - birth_year -4) %>%
  group_by(birth_year) %>%
  mutate(seniority = real_wage/max(if_else(age==20, real_wage, 0))) %>%
  ggplot(aes(age, seniority, color=as.factor(paste0(birth_year, " - ", birth_year+4))))+
  geom_line()+
  labs(x="年齢", y="指数(20-24歳を1.0とする)", color="コホート")
ggsave("./fig/seniority_curve.png")
