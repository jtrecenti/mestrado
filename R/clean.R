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

link <- 'http://www.buscabanco.com.br/AgenciasBancos.asp?uf=&ordem=banco&wtexto=&tipo=&origem=&natural='
bancos <- GET(link, encoding='latin1') %>%
  content('text') %>%
  html('UTF-8') %>%
  html_node(xpath="//table[@bgcolor='#003366']") %>%
  html_table(header=T, trim=T, fill=T, dec=',') %>%
  set_names(c('numero', 'nome', 'site', 'qtd_agencias')) %>%
  select(-site) %>%
  mutate(nome=gsub(' +', ' ', gsub('[./,\n()-]', '', rm_accent(toupper(nome)))),
         qtd_agencias=as.numeric(gsub('[.]', '', qtd_agencias))) %>%
  filter(!is.na(qtd_agencias)) %>%
  tbl_df %>%
  arrange(desc(qtd_agencias))

####################################################################################################
## DADOS TJSP
####################################################################################################

# Tarefa 1: Limpando metadados

# Tarefa 2: Extraindo informacoes dos textos

# Tarefa 3: Filtros

# Tarefa 4: Merge com bancos

# Tarefa 5: Merge com antiguidade dos juizes

####################################################################################################