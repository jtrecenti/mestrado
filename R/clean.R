# Clean data, final dataset

####################################################################################################
### ANTIGUIDADE DOS JUIZES
####################################################################################################

antig_juizes <- read.csv('antiguidade_juizes_tjsp.csv', as.is=T) %>%
  mutate(juiz=rm_accent(toupper(nome))) %>%
  filter(!duplicated(juiz)) %>%
  mutate(idade=today()-as.Date(mdy(nascimento)),
         idade=as.numeric(idade)/365) %>%
  select(juiz, idade, antiguidade) %>%
  arrange(desc(idade))

# Arrumando alguns
antig_juizes$juiz[str_detect(antig_juizes$juiz, 'CELINA DIETRICH')] <- 'CELINA DIETRICH TRIGUEIROS TEIXEIRA PINTO'
antig_juizes$juiz[str_detect(antig_juizes$juiz, 'EDWARD ALBERT')] <- 'EDWARD ALBERT LANCELOT D C CATERHAM WICKFIELD'
antig_juizes$juiz[str_detect(antig_juizes$juiz, 'SABRINA MARTINHO')] <- 'SABRINA MARTINHO'
antig_juizes$juiz[str_detect(antig_juizes$juiz, 'SALVETTI')] <- 'CLAUDIO SALVETTI D?ANGELO'

####################################################################################################
## BANCOS
####################################################################################################

####################################################################################################
## DADOS TJSP
####################################################################################################

# Tarefa 1: Limpando metadados

# Tarefa 2: Extraindo informacoes dos textos

# Tarefa 3: Filtros

# Tarefa 4: Merge com bancos

# Tarefa 5: Merge com antiguidade dos juizes

####################################################################################################