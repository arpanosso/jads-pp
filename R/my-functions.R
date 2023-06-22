my_regs<-function(x,y,x_est=2020){
  mod <- lm(y ~  x)
  a0 <- as.numeric(mod$coefficients[1])
  a1 <- as.numeric(mod$coefficients[2])
  y_est <- a0+a1*x_est
  return(a1)
}

is.integer64 <- function(x){
  class(x)=="integer64"
}

read_my_file <- function(file){
  da <- read_rds(file) %>%
    select(ano, mes, sigla_uf, id_municipio, id_municipio_6,
           indicador_servico_nutricao_proprio,
           indicador_servico_nutricao_terceirizado,
           indicador_servico_lactario_proprio,
           indicador_servico_lactario_terceirizado,
           indicador_servico_banco_leite_proprio,
           indicador_servico_banco_leite_terceirizado)
  print(file)
  return(da)
}

my_mean <- function(y1,y2,novo_ano,type="beta") {
  beta <- (((2000-2005)*(y1-(y1+y2)/2))+
             ((2010-2005)*(y2-(y1+y2)/2)))/((2000-2005)^2+(2010-2005)^2)
  alpha <- (y1+y2)/2 - beta*2005
  yest  <- alpha + beta*novo_ano

  if(type=="beta"){
    return(beta)
  } else {
    return(yest)
  }
}


previsao_atencao_basica <- function(col,ano_corte=2007,ano_final=2030,  janela = 3){
  df<-atencao_basica %>% filter(ano > ano_corte) %>%
    mutate(ano_mes = make_date(year=ano, month=mes, day = 1)) %>%
    group_by(ano_mes) %>%
    summarise(
      {{col}} := mean({{col}})
    )
  nome_y <- names(df)[2]
  names(df) <- c("x","y")
  mod <- lm(y ~ x, data=df)
  qanova <- summary.lm(mod)
  a0 <- qanova$coefficients[1]
  a1 <- qanova$coefficients[2]
  a0_err <- qanova$coefficients[3]
  a1_err <- qanova$coefficients[4]

  df_est <- expand.grid(ano=2021:ano_final, mes=1:12) %>%
    mutate(x = make_date(year=ano,month=mes,day=1)) %>%
    arrange(x)

  df_est$y <- rnorm(nrow(df_est),a0,a0_err) +
    rnorm(nrow(df_est),a1,a1_err)*as.numeric(df_est$x)

  df<-atencao_basica %>% filter(ano > 2007) %>%
    mutate(ano_mes = make_date(year=ano, month=mes, day = 1)) %>%
    group_by(ano_mes) %>%
    summarise(
      {{col}} := mean({{col}})
    )
  names(df) <- c("x","y")

  df$tipo <- "original"

  dados <- df_est$y
  media_mov_simples <- TTR::SMA(dados, n = janela)

  df_est$y <- media_mov_simples
  df_est$tipo <- "estimado"

  dfbindado <- rbind(df, df_est %>% select(x,y,tipo))

  x<-dfbindado$x
  y<-dfbindado$y
  mod_b <- lm(y ~x)
  a <- mod_b$coefficients[1]
  b <- mod_b$coefficients[2]
  c(a,b)


  dfbindado %>% mutate(y = ifelse(is.na(y),a+b*as.numeric(x),y)) %>%
    ggplot(aes(x = x, y=y, color=tipo)) +
    geom_line() +
    theme_bw() +
    geom_vline(xintercept = as.Date("2021-01-01"),color="red")+
    scale_color_manual(values=c("blue","black"))+
    labs(x="Tempo", y=nome_y)
}


previsao_sisvan_estab <- function(col,ano_corte=2007,ano_final=2030,  janela = 3){
  df<-df_aux %>% filter(ano > ano_corte) %>%
    mutate(ano_mes = make_date(year=ano, month=mes, day = 1)) %>%
    group_by(ano_mes) %>%
    summarise(
      {{col}} := mean({{col}})
    )
  df
  nome_y <- names(df)[2]
  names(df) <- c("x","y")
  mod <- lm(y ~ x, data=df)
  qanova <- summary.lm(mod)
  a0 <- qanova$coefficients[1]
  a1 <- qanova$coefficients[2]
  a0_err <- qanova$coefficients[3]
  a1_err <- qanova$coefficients[4]

  df_est <- expand.grid(ano=2023:ano_final, mes=1:12) %>%
    mutate(x = make_date(year=ano,month=mes,day=1)) %>%
    arrange(x)

  df_est$y <- rnorm(nrow(df_est),a0,a0_err) +
    rnorm(nrow(df_est),a1,a1_err)*as.numeric(df_est$x)

  df<-df_aux %>% filter(ano > 2007) %>%
    mutate(ano_mes = make_date(year=ano, month=mes, day = 1)) %>%
    group_by(ano_mes) %>%
    summarise(
      {{col}} := mean({{col}})
    )

  names(df) <- c("x","y")

  df$tipo <- "original"
  df
  dados <- df_est$y
  media_mov_simples <- TTR::SMA(dados, n = janela)

  df_est$y <- media_mov_simples
  df_est$tipo <- "estimado"

  dfbindado <-rbind(df, df_est %>% select(x,y,tipo))
  x<-dfbindado$x
  y<-dfbindado$y
  mod_b <- lm(y ~x)
  a <- mod_b$coefficients[1]
  b <- mod_b$coefficients[2]
  c(a,b)



dfbindado %>% mutate(y = ifelse(is.na(y),a+b*as.numeric(x),y)) %>%
  ggplot(aes(x = x, y=y, color=tipo)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = as.Date("2023-01-01"),color="red")+
  scale_color_manual(values=c("blue","black")) +
  labs(x="Tempo", y=nome_y)
}
