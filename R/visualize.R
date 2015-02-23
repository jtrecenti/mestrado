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

dim(d_tjsp)

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

