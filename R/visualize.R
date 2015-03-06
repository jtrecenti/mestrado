#___________________________________________________________________________________________________
# Descriptive Analysis
#___________________________________________________________________________________________________

#___________________________________________________________________________________________________
# Complete dataset
#___________________________________________________________________________________________________

library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)

d_tjsp <- readRDS('data/d_tjsp.rds') %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  )



# VOLUME PROCESSUAL
#___________________________________________________________________________________________________

# volume de dados estranho antes de 2010-01-01 e depois de 2014-07-01. Filtrar
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca)),
         ano_mes = sprintf('%04d-%02d', year(data_sentenca), month(data_sentenca))) %>%
  count(ano_mes) %>%
  ggplot(aes(x = ano_mes, y = n)) +
  geom_line(aes(group=1)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# depois de filtrar
# o volume de sentencas eh compativel com justica em numeros?
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  mutate(ano_mes = as.Date(sprintf('%04d-%02d-01', year(data_sentenca), month(data_sentenca)))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  count(ano_mes) %>%
  ggplot(aes(x = ano_mes, y = n)) +
  geom_vline(xintercept = as.numeric(as.Date(ymd('2010-01-01') + years(0:4))), 
             colour = 'blue', alpha=.6) +
  geom_vline(xintercept = as.numeric(as.Date(ymd('2010-01-01') + years(0:4) + months(6))), 
             colour = 'blue', alpha=.2) +
  geom_hline(aes(yintercept=median(n)), colour='red', alpha=.2) +
  geom_line() +
  geom_point(size=3, alpha=.4) +
  theme_bw() +
  scale_x_date(breaks = scales::date_breaks('1 month'), labels = scales::date_format("%y-%b")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CLASSE
#___________________________________________________________________________________________________
# 10 mais
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  count(classe, sort = T) %>%
  slice(1:10) %>%
  (knitr::kable)

# Agrupando em 'outros'
# Note que juntando todas as categorias de outros, ainda temos menos observacoes que nas outras
# categorias
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  mutate(n_tot=n()) %>%
  group_by(classe) %>%
  mutate(n = n(), classe2 = ifelse(n / n_tot < .02, 'outros', classe)) %>%
  ungroup %>%
  ggplot(aes(x = classe2)) +
  geom_bar(colour = 'black', alpha = .2) +
  #geom_text(aes(x = classe2, y = ..count.., label= (..count..) / sum(..count..)), vjust = 1) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ASSUNTO
#___________________________________________________________________________________________________

# 15 mais
# muitos vazios, quase 10%!!
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  count(assunto, sort = T) %>% 
  mutate(prop = round(n / sum(n) * 100, 2)) %>%
  slice(1:15) %>%
  (knitr::kable)


# 5 mais, agrupado por classe
# muitos vazios, quase 10%!!
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  count(classe, assunto, sort=T) %>% 
  group_by(classe) %>%
  mutate(prop = round(n / sum(n) * 100, 2)) %>%
  slice(1:5) %>%
  (knitr::kable)


# Agrupando em 'outros'
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  mutate(n_tot=n()) %>%
  group_by(assunto) %>%
  mutate(n = n(), assunto2 = ifelse(n / n_tot < .02, 'outros', assunto)) %>%
  ungroup %>%
  ggplot(aes(x = assunto2)) +
  geom_bar(colour = 'black', alpha = .9) +
  facet_wrap(~ classe, ncol = 3, scales = 'free_y') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Foro / Vara
#___________________________________________________________________________________________________

# Foro
# Santo Amaro, Itaquera e Central sao os mais volumosos. Por que?
# Nao estou interessado no foro da fazenda publica e acidentes
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  count(foro, sort = T) %>% 
  mutate(prop = round(n / sum(n) * 100, 2)) %>%
  (knitr::kable)

# Vara
# Possivel notar que o juizado especial civel eh volumoso em todos os foros
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  count(foro, vara, sort = T) %>% 
  mutate(prop = round(n / sum(n) * 100, 2)) %>%
  slice(1:3) %>%
  (knitr::kable)

# Tirando varas de familia e infancia e juventude
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  count(foro, vara, sort = T) %>% 
  ungroup %>%
  arrange(n) %>%
  slice(1:20) %>%
  (knitr::kable)

# Tipos das varas
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  count(foro, tipo_vara) %>%
  arrange(foro, tipo_vara) %>%
  (knitr::kable)

# Mapeamento
#___________________________________________________________________________________________________

# Tempo dos processos
#___________________________________________________________________________________________________

# Tempo da distribuicao ate a sentenca
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  select(tempo) %>%
  ggplot(aes(x = tempo)) +
  geom_histogram(aes(y = ..density..), fill = 'transparent', colour = 'black') +
  geom_density(alpha = .2, fill = 'royalblue') +
  theme_bw()

# tempos negativos
# na verdade, queremos a data do primeiro andamento, pois no TJSP o dado
# data_distribuicao eh atualizado.
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  filter(tempo < 0) %>%
  arrange(tempo) %>%
  select(n_processo, data_sentenca, data_distribuicao, tempo)
  
# tempos muito grandes
# sao filhos de deus...
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  filter(tempo > 5000) %>%
  arrange(desc(tempo)) %>%
  select(n_processo, data_sentenca, data_distribuicao, tempo)

# Tempos muito pequenos (em modulo)
# Tambem sao erros por conta do metadado.
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  filter(abs(tempo) <= 10) %>%
  arrange(abs(tempo)) %>%
  select(n_processo, data_sentenca, data_distribuicao, tempo)

# Analise dos tempos assumindo que esta tudo certo
#___________________________________________________________________________________________________

# Comparando por classe
# Claramente JEC eh mais rapido, e estranhamente processo sumario nao eh mais rapido
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  ggplot(aes(x = tempo)) +
  geom_histogram(aes(y = ..density..), fill = 'transparent', colour = 'black') +
  geom_density(alpha = .2, fill = 'royalblue') +
  facet_wrap(~ classe) +
  theme_bw()

# Comparando por foro e tipo de vara
# Lapa, Vila Prudente e Itaquera com tempos grandes. Pinheiros, Tatuape com tempos pequenos
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  ggplot(aes(x = foro, y = tempo, fill = tipo_vara)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comparando por vara
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  group_by(vara, foro) %>%
  summarise(n = n(), media = mean(tempo), desv_pad = sd(tempo), mediana = median(tempo)) %>%
  ungroup %>%
  arrange(desc(mediana)) %>%
  slice(1:20) %>%
  (knitr::kable)

# Comparando por assunto
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  mutate(n_tot = n()) %>%
  group_by(assunto) %>%
  mutate(n = n(), assunto2 = ifelse(n / n_tot < .01, 'outros', assunto)) %>%
  ungroup %>%
  ggplot(aes(x = assunto2, y = tempo, fill = tipo_vara)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comparando por assunto - JEC
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  filter(tipo_vara == 'JEC') %>%
  mutate(n_tot = n()) %>%
  group_by(assunto) %>%
  mutate(n = n(), assunto2 = ifelse(n / n_tot < .001, 'outros', assunto)) %>%
  ungroup %>%
  group_by(assunto2) %>%
  summarise(n = n(), media = mean(tempo), desv_pad = sd(tempo), mediana = median(tempo)) %>%
  ungroup %>%
  arrange(desc(mediana)) %>%
  (knitr::kable)

# Comparando por assunto - Civel
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  filter(tempo >= 50, tempo <= 3650) %>% # corrigir
  filter(tipo_vara == 'Cível') %>%
  mutate(n_tot = n()) %>%
  group_by(assunto) %>%
  mutate(n = n(), assunto2 = ifelse(n / n_tot < .01, 'outros', assunto)) %>%
  ungroup %>%
  group_by(assunto2, classe) %>%
  summarise(n = n(), media = mean(tempo), desv_pad = sd(tempo), mediana = median(tempo)) %>%
  ungroup %>%
  arrange(desc(mediana)) %>%
  (knitr::kable)

# Analise do valor dos processos
#___________________________________________________________________________________________________

# Densidades comparando JEC e Civel
# Possivel notar que no JEC ha um limitador no valor da acao, o que faz sentido
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  ) %>%
  ggplot(aes(x = valor_acao_num, fill = tipo_vara)) +
  geom_histogram(aes(y = ..density..), colour = 'black', position = 'dodge') +
  geom_density(alpha = .3) +
#   facet_wrap(~ tipo_vara) +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

# Densidades comparando classes
# Possivel notar um limitador no procedimento sumario tambem.
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  ) %>%
  ggplot(aes(x = valor_acao_num, fill = classe)) +
  geom_histogram(aes(y = ..density..), colour = 'black', position = 'dodge') +
  geom_density(alpha = .3) +
  facet_wrap(~ classe, ncol = 1) +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

# Aparentemente nao ha relacao do valor da acao com o tempo
# estranho notar a massa de tempo 5000 no JEC
# no grafico log log interessante notar alguns tempos fixos entre 0 e 100
# interessante notar tambem alguns valores fixados de causa
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  ) %>%
  ggplot(aes(y = valor_acao_num, x = tempo, colour = tipo_vara)) +
  geom_point(alpha=.3) +
  scale_y_log10(labels = scales::dollar) +
  scale_x_log10() +
  theme_bw()

# Aparentemente nao ha relacao do valor da acao com o tempo
# estranho notar a massa de tempo 5000 no JEC
# no grafico log log interessante notar alguns tempos fixos entre 0 e 100
# interessante notar tambem alguns valores fixados de causa
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  ) %>%
  ggplot(aes(y = valor_acao_num, x = data_distribuicao, colour = classe)) +
  geom_point(alpha=.3) +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()

# Da para notar que os processos estranhamente demorados nos JECs foram distribuidos em 2000.
d_tjsp %>%
  mutate(data_sentenca = as.Date(dmy(data_sentenca))) %>%
  filter(data_sentenca >= as.Date('2010-01-01'), data_sentenca <= as.Date('2014-08-31')) %>%
  filter(classe %in% c('Procedimento do Juizado Especial Cível', 
                       'Procedimento Ordinário',
                       'Procedimento Sumário')) %>%
  filter(!foro %in% c('Foro Central - Fazenda Pública/Acidentes', 
                      'Foro Distrital de Parelheiros')) %>%
  filter(!str_detect(vara, 'Família|Juventude|Registros|Falências')) %>%
  mutate(data_distribuicao = as.Date(dmy(data_distribuicao))) %>%
  mutate(tempo = as.numeric(data_sentenca - data_distribuicao)) %>%
  mutate(tipo_vara = ifelse(str_detect(vara, 'special|tinerante'), 'JEC', 'Cível')) %>%
  mutate(foro = ifelse(foro %in% 'Foro Central Juizados Especiais Cíveis', 
                       'Foro Central Cível', foro)) %>%
  mutate(valor_acao_num = as.numeric(
    str_replace_all(str_replace_all(valor_acao, '[R$ .]', ''), ',', '.'))
  ) %>%
  ggplot(aes(y = data_sentenca, x = data_distribuicao, colour = classe)) +
  geom_point(alpha = .3) +
  facet_wrap(~ classe, ncol = 1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = as.numeric(as.Date(ymd('2010-01-01') + years(0:5))), alpha = .3) +
  theme_bw()

# Requeridos
#___________________________________________________________________________________________________

d_tjsp %>%
  mutate(reqdo_limpo = toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 \n]', '', rm_accent(reqdo))))) %>%
  count(reqdo_limpo, sort = T) %>%
  head(20) %>%
  (knitr::kable)

d_tjsp %>%
  mutate(reqdo_limpo = toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 \n]', '', rm_accent(reqdo))))) %>%
  filter(year(data_sentenca) == 2014) %>%
  count(reqdo_limpo, sort = T) %>%
  head(20) %>%
  (knitr::kable)


d_tjsp %>%
  mutate(reqdo_limpo = toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 \n]', '', rm_accent(reqdo))))) %>%
  filter(year(data_sentenca) == 2014) %>%
  count(reqdo_limpo, sort = T) %>%
  View


# Classificacao dos requeridos
# BANCOS: BANCO DO BRASIL, ITAU, BRADESCO, SANTANDER. 
# TELEFONIA: TIM, VIVO, CLARO, NET, NEXTEL

bradesco <- 'BRADESCO SA|BRADESCAR|BRADESCO$|BRADESCO CARTOES|BRADESCO FINANC'
itau <- 'UNIBANCO SA$|UNIBANCO ITAU|ITAU UNIBANCO BANCO MULTIPLO SA|ITAU CARD|ITAUCARD|ITAU UNIBANCO SA|ITAUCARD SA|ITAU$|UNIBANCO$|ITAU SA|ITAUCARD$|ITAUUNIBANCO|^ITAU$|ITAU FINAN|ITAU UNIBANCO F|FINANCEIRA ITAU'
santander <- 'SANTANDER$|SANTANDER [^L]|SANTANDER[SA]'
bb <- 'BANCO DO BRASIL'
tim <- 'TIM CELUL?AR|^TIM SA|TIM BRASIL SA|TIM DO BRASIL SA|^TIM$| TIM$|TIM OPERADORA|TIM TELEFONIA'
claro <- 'CLARO$|CLARO [^A]|EMBRATEL'
net <- '^NET$|^NET | NET S|CENTRAL DA NET'
nextel <- 'NEXTEL'
vivo <- 'VIVO|TELESP|TELEFONICA'
eletropaulo <- 'ELETROP'

pj <- c('TELLERINA', 'ALLIANZ', 'INDÚSTRIA', 'CONSULTORIA',
        ' ME$', 'LTDA', 'CONDOMINIO', 'DEF PUB', 'LOGISTICA',
        'SPEED BEE', 'ASSOCIACAO', 'EUROTUBOS', 'TRANSPORTES') %>% paste(collapse='|')


d_tjsp_empresas <- d_tjsp %>%
  filter(year(data_sentenca) %in% 2014) %>%
  mutate(reqdo_limpo = toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 \n]', '', rm_accent(reqdo)))),
         reqte_limpo = toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 \n]', '', rm_accent(reqte))))) %>%
  filter(str_detect(
    reqdo_limpo,
    paste(bradesco, itau, santander, bb, tim, claro, net, nextel, vivo, eletropaulo, sep = '|'))
  ) %>%
  filter(!str_detect(reqte_limpo, pj)) %>%
  mutate(q_bradesco = ifelse(str_detect(reqdo_limpo, bradesco), 'BRADESCO', ''),
         q_itau = ifelse(str_detect(reqdo_limpo, itau), 'ITAU', ''),
         q_santander = ifelse(str_detect(reqdo_limpo, santander), 'SANTANDER', ''),
         q_bb = ifelse(str_detect(reqdo_limpo, bb), 'BB', ''),
         q_tim = ifelse(str_detect(reqdo_limpo, tim), 'TIM', ''),
         q_claro = ifelse(str_detect(reqdo_limpo, claro), 'CLARO', ''),
         q_net = ifelse(str_detect(reqdo_limpo, net), 'NET', ''),
         q_nextel = ifelse(str_detect(reqdo_limpo, nextel), 'NEXTEL', ''),
         q_vivo = ifelse(str_detect(reqdo_limpo, vivo), 'VIVO', ''),
         q_eletropaulo = ifelse(str_detect(reqdo_limpo, eletropaulo), 'ELETROPAULO', '')) %>%
  rowwise %>%
  mutate(empresa = str_trim(paste(q_bradesco, q_itau, q_santander, q_bb, q_tim, q_claro, q_net, 
                                  q_nextel, q_vivo, q_eletropaulo))) %>%
  ungroup %>%
  filter(!str_detect(empresa, ' ')) %>%
  tbl_df %>%
  select(-starts_with('q_'))

d_tjsp_empresas %>%
  ggplot(aes(x = log10(valor_acao_num+1), fill=tipo_vara)) +
  geom_density(alpha=.2) +
  facet_wrap(~empresa) +
  theme_bw()

d_tjsp_empresas %>%
  count(empresa, tipo_vara, sort=T) %>%
  (knitr::kable)

# Classificacao por palavras-chave
#___________________________________________________________________________________________________

library(tm)

meses <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho",
           "agosto", "setembro", "outubro", "novembro", "dezembro")

banned_words_direito <- c("a", "ajuiz", "aleg", "art", "artig", "autor",
                          "cdig", "direit", "fls", "inicial", "julg", "juiz", "juz", "justic",
                          "lei", "peti", "process", "ru", "sentenc", "vist", "vot", 'tribunal',
                          'classe', 'assunto', 'part', 'jur', 'dan', 'contrat', 'pel', 'par', 'valor')

banned_words <- c(stopwords("portuguese"), meses, banned_words_direito)

# txt_interesse <- gsub('[^0-9]', '', list.files('data/txt'))
# txt_interesse <- txt_interesse[txt_interesse %in% d_tjsp_empresas$n_processo]
# dir.create('data/txt_empresas')
# file.copy(sprintf('%s/%s.txt', 'data/txt/', txt_interesse), 'data/txt_empresas')

ds <- DirSource('data/txt_empresas/', encoding='UTF-8')
txt <- VCorpus(ds, readerControl = list(reader=readPlain, language='pt-br'))

banned_words <- c('fls', 'art.', 'artigo', 'autos', 'acao', 'caso', 
                  'conforme', 'civel', 'codigo', 'data', 'desde', 'direito', 'documentos',
                  'especial', 'fato', 'forma', 'juiz', 'juizado', 'lei', 'mes', 'nome', 
                  'sao paulo', 'processo', 'presente', 'qualquer', 'recurso', 'relacao',
                  'requerido', 'sentenca', 'termos', 'tribunal', 'assim', 'ainda',
                  'partes', 'brasil')

banned_words <- c('direito', 'sentenca', 'processo', 'sao paulo', 'vistos', 'termos', 'autos', 
                  'lei', 'relatorio', 'acao', 'civel', 'partes', 'codigo', 'artigo', 'art', 
                  'classe', 'forma', 'assuntoprocedimento', 'juizado', 'civil', 'conforme',
                  'recurso', 'sendo', 'assim', 'data', 'presente', 'fls', 'brasil', 'desde',
                  'justica', 'qualquer', 'digialmente', 'impressao', 'requeridobanco', 'que')


deleta <- function(x, pattern) gsub(pattern, "", x)

txt2 <- txt %>%
  tm_map(removePunctuation, preserve_intra_word_dashes=T) %>%
  tm_map(content_transformer(deleta), 'ª|º') %>%
  tm_map(content_transformer(rm_accent)) %>%  
  tm_map(content_transformer(tolower)) %>%
  #tm_map(removeWords, banned_words) %>%
  #tm_map(removeWords, stopwords('portuguese')) %>%
  tm_map(stripWhitespace)





# WORD CLOUD
#___________________________________________________________________________________________________

wordcloud_txt <- function(txt, i, min = 1, max = 1, min_freq = 2) {
  tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
  tf_bi <- termFreq(txt2[[i]], control = list(tokenize = tokenizer)) %>%
    data_frame(nome = names(.), freq = .)
  wordcloud(tf_bi$nome, tf_bi$freq, 
            min.freq = min_freq, 
            random.color = TRUE, 
            rot.per = 0, 
            fixed.asp = FALSE)
}

s <- sample(seq_len(length(txt2)), 1)
wordcloud_txt(txt, s, min = 3, max = 3, min_freq = 2)

# WORD TREE
#___________________________________________________________________________________________________
# outro dia



# RECUPERANDO INFORMACOES
#___________________________________________________________________________________________________

txt_limpo <- txt %>%
  tm_map(removePunctuation, preserve_intra_word_dashes=T) %>%
  tm_map(content_transformer(deleta), 'ª|º') %>%
  tm_map(content_transformer(rm_accent)) %>%  
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, banned_words) %>%
  tm_map(removeWords, stopwords('portuguese')) %>%
  tm_map(stripWhitespace)


# Aproveitando estrutura dos documentos
#___________________________________________________________________________________________________

d_txt <- bind_rows(lapply(1:length(txt), function(x) {
  data_frame(n_processo = gsub('[^0-9]', '', txt[[x]]$meta$id), 
             txt = paste(txt[[x]]$content, collapse='\n\n'),
             txt2 = paste(txt2[[x]]$content, collapse='\n\n'))
}))

d_txt <- d_tjsp_empresas %>%
  select(empresa, tipo_vara, n_processo) %>%
  inner_join(d_txt, 'n_processo')

# JEC

d_txt_jec <- d_txt %>%
  filter(tipo_vara == 'JEC') %>%
  mutate(termo_audiencia = str_detect(txt2, 'termo de audiencia'))

d_txt_jec %>%
  filter(termo_audiencia) %T>% print %>%
  sample_n(1) %>%
  with(txt) %>%
  cat

# Civel

d_txt_civ <- d_txt %>%
  filter(tipo_vara == 'Cível') %>%
  mutate(termo_audiencia = str_detect(txt2, 'termo de audiencia'))

d_txt_civ %>%
  filter(termo_audiencia) %T>% print %>%
  sample_n(1) %>%
  with(txt) %>%
  cat

d_txt_civ %>%
  filter(!termo_audiencia) %T>% print %>%
  sample_n(1) %>%
  with(txt) %>%
  cat

tx <- d_txt_civ %>%
  filter(!termo_audiencia) %>%
  sample_n(1) %>%
  with(txt)

processa_processo <- function(tx) {
  linhas <- unlist(str_split(tx, '\n\n'))
  
  vistos <- first(grep('Vistos', linhas))
  pric <- last(grep('P\\.?R\\.?I\\.?C?', linhas))
  
  linhas <- linhas[(vistos + 1):(pric - 1)]
  
  decido <- first(grep('Decido.', linhas))
  
  decisao <- linhas[str_detect(linhas, 'Ante o exposto')]
  
  d <- dplyr::data_frame(fatos = fatos,
                         argumentos = argumentos,
                         decisao = decisao)
  
}


#___________________________________________________________________________________________________


dano_material <- 'danos?([a-z ]+)?material?(is)?|danos?([a-z ]+)? patrimonial?(is)?'
dano_moral <- 'danos?([a-z ]+)?moral?(is)?|danos?([a-z ]+)? extrapatrimonial?(is)?'
seguro <- ' segur'
honorarios <- 'honorario'
procedente <- '(julgo|declaro) (totalmente )?procedente'
improcedente <- '(julgo|declaro) (totalmente )?improcedente'
extinto <- '(julgo|declaro) extinto?a?'
parc_procedente <- '(julgo|declaro) parcialmente procedente'
procedente_parte <- '(julgo|declaro) procedente em parte'
tutela_antecipada <- 'tutela antecipada'
plano_saude <- 'plano de saude'
inexistencia_debito <- 'inexistencia de debito|inscricao indevida|indevidamente escrito|serasa|inexigibilidade de debito|debito e inexigivel'
dpvat <- 'dpvat'
acordo <- 'homolog'
conciliacao_infrutifera <- 'infrutifera'
conciliacao_frutifera <- ' frutifera'
calcada <- 'calcada'
audiencia <- 'audiencia'

# Novas palavras-chave
inicio_decisao <- ''
onus <- 'onus'
collor <- 'collor|bresser|plano verao'
hipossuf <- 'hiposs'
em_dobro <- 'em dobro'
prescricao <- 'prescric'
rel_dano_moral <- 'abalo|psiquico|aborr|sofrim'
dissabor <- 'dissabor'
espolio <- 'espolio'
cujus <- 'cujus'
lei_jec <- '9099/95|9\\.099/95'
consumo <- 'consum'
inscr_indevida <- 'serasa|spc|inscricao indevida|cadastro de inadimp|scpc'
inadimpl <- 'inadimpl'


tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))

tdm <- txt2[1:10] %>%
  TermDocumentMatrix(control = list(tokenize = tokenizer)) %>%
  removeSparseTerms(.1) %>%
  as.matrix %>% 
  data.frame %>%
  tbl_df %>%
  add_rownames('palavra') %>%
  gather(processo, n, -palavra) %>%
  filter(n > 0)


#___________________________________________________________________________________________________


tdm <- txt2q %>%
  TermDocumentMatrix(control = list(tokenize = BigramTokenizer)) %>%
  removeSparseTerms(.1) %>%
  as.matrix %>% 
  data.frame %>%
  mutate(palavra=row.names(.)) %>% 
  tbl_df %>%
  gather(processo, n, -palavra) %>%
  filter(n > 0)

tdm %>%
  group_by(palavra) %>%
  summarise(n_docs=n()) %>%
  ungroup %>%
  arrange(desc(n_docs)) %>%
  head(30)


obj %>%
  group_by(palavra) %>%
  summarise(n=sum(n)) %>%
  ungroup %>%
  arrange(desc(n))












