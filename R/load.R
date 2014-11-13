## Downloads data, load packages

# library(tjsp)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(ggplot2)
library(httr)
# library(SnowballC)
# library(tm)
# library(topicmodels)
# library(e1071)
# library(FactoMineR)

set_names <- function(.data, n) {
  names(.data) <- n
  .data
}

add_key <- function(d, re, lab) {
  d[[lab]] <- ifelse(str_detect(d$txt2, perl(re)), 's', 'n')
  return(d)
}

rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

pega_bancos <- function() {
  link <- 'http://www.buscabanco.com.br/AgenciasBancos.asp?uf=&ordem=banco&wtexto=&tipo=&origem=&natural='
  bancos <- GET(link, encoding='latin1') %>%
    content('text') %>%
    html('UTF-8') %>%
    html_node(xpath="//table[@bgcolor='#003366']") %>%
    html_table(header=T, trim=T, fill=T, dec=',') %>%
    set_names(c('numero', 'nome', 'site', 'qtd_agencias')) %>%
    select(-site) %>%
    mutate(nome=gsub(' +', ' ', gsub('[./,\n()-]', '', toupper(nome))),
           qtd_agencias=as.numeric(gsub('[.]', '', qtd_agencias))) %>%
    filter(!is.na(qtd_agencias)) %>%
    tbl_df %>%
    arrange(desc(qtd_agencias))
  bancos
}

####################################################################################################
### ANTIGUIDADE DOS JUIZES
####################################################################################################

# pegar de mais fontes?
antig_juizes <- read.csv('data/lista_antiguidade_final.csv', as.is=T, encoding='UTF-8')

####################################################################################################
## BANCOS
####################################################################################################

bancos <- pega_bancos()
save(bancos, file='data/bancos.RData')

####################################################################################################
## DADOS TJSP
####################################################################################################

# Processos da area civel com palavra-chave dano/danos pesquisados ate xx/xx/2014
load("data/d_tjsp.RData")

## RECLASSIFICACAO DOS REQUERIDOS
banco <- c('BANCO', 'BRADESCO', 'ITAU', 'SANTANDER', 'VOTORANTIM',
           'SAFRA', 'CITI', 'JP MORGAN', 'DAYCOVAL') %>% paste(collapse='|')

nao_banco <- c('SANGUE', 'CAPITAUS', 'VIDA', 'SAUDE', 'BRADESCO ADM', 'ITAU ADM', 'SERASA',
               'SEGURO', 'UNIBANCO AIG', 'ARREND', 'ARRED', 'ARRENA', 'COOPER', 'ITAU CDB',
               'ITAUTEC', 'TAII', 'FINANCEIRA ITAU', 'BANCO SANTOS') %>% paste(collapse='|')

pj <- c('TELLERINA', 'ALLIANZ', 'INDÚSTRIA', 'CONSULTORIA',
        ' ME$', 'LTDA', 'CONDOMINIO', 'DEF PUB', 'LOGISTICA',
        'SPEED BEE', 'ASSOCIACAO', 'EUROTUBOS', 'TRANSPORTES') %>% paste(collapse='|')

d_meta <- d_meta_raw %>%
  filter(!area %in% 'Criminal', !is.na(area)) %>%
  mutate(reqdo_limpo=toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 ]', '', rm_accent(reqdo)))),
         reqte_limpo=toupper(gsub('\\s+', ' ', gsub('[^A-Za-z0-9 ]', '', rm_accent(reqte)))),
         n_partes=str_count(reqdo, '\n')+1) %>%
  filter(str_detect(reqte_limpo, ' ')) %>%
  mutate(reqdo_nao_banco=str_detect(reqdo_limpo, nao_banco),
         reqdo_banco=str_detect(reqdo_limpo, banco),
         reqte_pj=str_detect(reqte_limpo, pj)) %>%
  filter(reqdo_banco, !reqdo_nao_banco, !reqte_pj) %>%
  mutate(reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO BMC|DE DESCONTOS SA|BANCO BRADESCO|FINASA|BRADESCO SA|^BRADESCO$'), 'BANCO BRADESCO SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO ITAU|^ITAU$|UNIBANCO|FININVEST|ITAU SA'), 'ITAU UNIBANCO SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'ITAUCARD'), 'BANCO ITAUCARD SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'SANTADER|BANESPA|BANCO SANTANDER|SANTANDER SA|SANTANDER BRASIL|SUDAMER|AMRO|BANCO REAL'), 'BANCO SANTANDER BRASIL SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO BRADESCARD SA'), 'BANCO BRADESCO CARTOES SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO IBI'), 'IBIBANK SA BANCO MULTIPLO', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'NOSSA CAIXA'), 'CAIXA ECONOMICA FEDERAL', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO BV FINANCEIRA|VOTORANTIM'), 'BANCO VOTORANTIM SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO DO BRASIL|BANCO BRASIL SA'), 'BANCO DO BRASIL SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO BANKPAR'), 'BANCO BANKPAR SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BRADESCO CARTOES'), 'BANCO BRADESCO CARTOES SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO BMG|SCHAHIN'), 'BANCO BMG SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO B ?G ?N'), 'BANCO CETELEM SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO GE CAPITAL SA'), 'BANCO CIFRA SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'HSBC'), 'HSBC BANK BRASIL SA BANCO MULTIPLO', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'FINANCEIRA ITAU CBD'), 'BANCO ITAULEASING SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'BANCO CRUZEIRO DO SUL|BANCO PAN|BANCO PANAMERICANO'), 'BANCO PANAMERICANO SA', reqdo_limpo),
         reqdo_limpo=ifelse(str_detect(reqdo_limpo, 'CARREFOUR'), 'BANCO CSF SA', reqdo_limpo)) %>%
  inner_join(bancos, c('reqdo_limpo'='nome')) %>%
  select(-f, -area, -processo, -reqdo, -reqdo_nao_banco, -reqdo_banco, -numero) %>%
  mutate(reqte_tem_adv=!is.na(adv_reqte), reqdo_tem_adv=!is.na(adv_reqdo)) %>%
  inner_join(d_sp, 'n_processo') %>%
  select(-juiz, -classe, -assunto) %>%
  rename(juiz=juiz_p, classe=classe_p, assunto=assunto_p) %>%
  mutate(juiz=rm_accent(toupper(juiz))) %>%
  left_join(antig_juizes, 'juiz')

#################################################################################################

meses <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho",
           "agosto", "setembro", "outubro", "novembro", "dezembro")
banned_words_direito <- c("a", "ajuiz", "aleg", "art", "artig", "autor",
                          "cdig", "direit", "fls", "inicial", "julg", "juiz", "juz", "justic",
                          "lei", "peti", "process", "ru", "sentenc", "vist", "vot", 'tribunal',
                          'classe', 'assunto', 'part', 'jur', 'dan', 'contrat', 'pel', 'par', 'valor')
banned_words <- c(stopwords("portuguese"), meses, banned_words_direito)


## PALAVRAS
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

d_aux <- d_meta %>%
  select(n_processo, txt) %>%
  mutate(txt1=str_trim(tolower(gsub('\n', ' ', gsub("assunto|procedimento|ordinário", "", txt)))),
         txt2=gsub(' +', ' ', rm_accent(tolower(txt1))))

d_txt <- d_aux %>%
  add_key(dano_material, 're_dano_material') %>%
  add_key(dano_moral, 're_dano_moral') %>%
  add_key(seguro, 're_seguro') %>%
  add_key(inexistencia_debito, 're_inexistencia_debito') %>%
  add_key(honorarios, 're_honorarios') %>%
  add_key(procedente, 're_procedente') %>%
  add_key(improcedente, 're_improcedente') %>%
  add_key(extinto, 're_extinto') %>%
  add_key(parc_procedente, 're_parc_procedente') %>%
  add_key(procedente_parte, 're_procedente_parte') %>%
  add_key(acordo, 're_acordo') %>%
  add_key(tutela_antecipada, 're_tutela_antecipada') %>%
  add_key(plano_saude, 're_plano_saude') %>%
  add_key(conciliacao_infrutifera, 're_conciliacao_infrutifera') %>%
  add_key(conciliacao_frutifera, 're_conciliacao_frutifera') %>%
  add_key(audiencia, 're_audiencia') %>%
  # NOVAS PALAVRAS
  add_key(onus, 're_onus') %>%
  add_key(collor, 're_collor') %>%
  add_key(hipossuf, 're_hipossuf') %>%
  add_key(em_dobro, 're_em_dobro') %>%
  add_key(prescricao, 're_prescric') %>%
  add_key(dissabor, 're_dissabor') %>%
  add_key(espolio, 're_espolio') %>%
  add_key(cujus, 're_cujus') %>%
  add_key(lei_jec, 're_lei_jec') %>%
  add_key(consumo, 're_consumo') %>%
  add_key(inscr_indevida, 're_inscr_indevida') %>%
  add_key(inadimpl, 're_inadimpl')

d_txt$valor_sentenca <- d_txt %>%
  with(txt2) %>%
  str_match_all('r\\$ ?([0-9]+,[0-9]+).*honorario') %>%
  sapply(function(x) {if(length(dim(x))>0) {return(x[dim(x)[1],2])}else {return(NA)}}) %>%
  str_replace_all('\\.', '') %>%
  str_replace_all(',', '.') %>%
  as.numeric

d_txt$valor_sentenca <- ifelse(d_txt$re_improcedente=='s'|d_txt$re_extinto=='s'|d_txt$re_acordo=='s',
                               0, d_txt$valor_sentenca)

####################################################################################################
### DESCRITIVA
####################################################################################################

# d_txt %>% summarise_each(funs(sum(.=='s')), starts_with('re_')) %>% t
#
# mostra <- function(d, x) {
#   eval(substitute(d %>% filter(VAR=='s') %>% sample_n(1) %>% with(txt) %>% cat(file='arquivo.txt'),
#                   list(VAR=as.name(x))))
# }
# d_txt %>% mostra('re_collor')
#
#
# cats = d_txt %>% select(starts_with('re_')) %>% apply(2, function(x) nlevels(as.factor(x)))
# mca1 <- d_txt %>% select(starts_with('re_')) %>% mutate_each(funs(factor)) %>% MCA(graph = FALSE)
# mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
# mca1_obs_df = data.frame(mca1$ind$coord)
# ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_point(colour = "gray50", alpha = 0.1) +
#   geom_density2d(colour = "gray80") +
#   geom_text(data=mca1_vars_df,
#             aes(x= Dim.1, y=Dim.2, label=rownames(mca1_vars_df), colour=Variable)) +
#   scale_colour_discrete(name = "Variable") +
#   theme_bw()
#
# require(bnlearn)
# learning.test
#
# d_reg <- d_txt %>% select(starts_with('re_')) %>% mutate_each(funs(factor)) %>% data.frame
# pdag <- d_reg %>% iamb
# pdag
# plot(pdag)

# COISAS QUE EU SEI QUALITATIVAMENTE
#
# tem muito mais dano moral do que material
# o dano material e o dano moral andam juntos
# não sei se os termos de audiência ocorrem somente quando o processo é do JEC
# processos do JEC não têm relatório (mais difícil de obter informação)
# - questão do collor: problema da poupança nos planos collor, verão e bresser
# - questão do onus da prova: quando estamos falando de direito do consumidor, o onus de prova é do réu

# A=ato ilícito
# Da=Dano
# V=Vara
# T=tentativa de acordo
# M=antiguidade/idade
# D=decisão
# L=LOSS
# E=Empresa
# C=?

####################################################################################################
## Analise 1.1 cadastro de inadimplentes.
####################################################################################################
d_inadimpl <- d_meta %>%
  inner_join(d_txt, 'n_processo') %>%
  mutate(juiz=rm_accent(toupper(juiz))) %>%
  filter(re_inadimpl=='s', str_detect(distribuicao, 'Livre')) %>%
  select(-re_inadimpl, -starts_with('txt'), -starts_with('adv_'), -assunto,
         -reqte, -reqte_limpo, -reqte_pj, -comarca, -cod_sentenca) %>%
  mutate(valor_da_acao=as.numeric(gsub(',', '.', gsub('[^0-9,]', '', gsub('\\.', '', valor_da_acao))))) %>%
  mutate(resultado=ifelse(re_procedente_parte=='s'|re_parc_procedente=='s', 'procedente_parte',
                          ifelse(re_procedente=='s', 'procedente',
                                 ifelse(re_improcedente=='s', 'improcedente',
                                        ifelse(re_acordo=='s', 'acordo',
                                               ifelse(re_extinto=='s', 'extinto',
                                                      'outro')))))) %>%
  select(-re_procedente_parte, -re_parc_procedente, -re_procedente,
         -re_improcedente, -re_acordo, -re_extinto) %>%
  mutate(conciliacao=ifelse(re_conciliacao_infrutifera=='s', 'infrutifera',
                            ifelse(re_conciliacao_frutifera=='s', 'frutifera',
                                   NA))) %>%
  select(-re_conciliacao_infrutifera, -re_conciliacao_frutifera, -re_honorarios) %>%
  mutate(dano=ifelse(re_dano_material=='s' & re_dano_moral=='s', 'ambos',
                     ifelse(re_dano_material=='s', 'material',
                            ifelse(re_dano_moral=='s', 'moral',
                                   'nenhum')))) %>%
  filter(dano!='nenhum') %>%
  select(-re_dano_material, -re_dano_moral) %>%
  mutate(idade=cut(idade, breaks=c(30, 45, 55, Inf)),
         antiguidade=cut(antiguidade, breaks=c(0,15,20,25,Inf)),
         valor_acao=valor_da_acao,
         valor_acao_cat=cut(valor_acao, breaks=c(0, 1000, 5000, 10000, 30000, Inf))) %>%
  mutate(jec=ifelse(str_detect(vara, 'Juizado') & reqte_tem_adv, 'jec_com_adv',
                    ifelse(str_detect(vara, 'Juizado'), 'jec_sem_adv',
                           'justica_comum')),
         regional=ifelse(str_detect(foro, 'Central'), 'central', 'regional'),
         n_partes=factor(n_partes)) %>%
  select(-foro, -vara, -reqte_tem_adv, -classe,
         -valor_da_acao, -re_espolio, -re_cujus, -re_plano_saude) %>%
  mutate(tempo=as.numeric(dmy(data_disp)-dmy(distribuicao))/60/60/24/365,
         tempo_cat=ifelse(tempo <= 0, NA, tempo),
         tempo_cat=cut(tempo, breaks=c(0,.5,1,2,Inf))) %>%
  select(-data_disp, -distribuicao) %>%
  mutate(qtd_agencias=cut(qtd_agencias, breaks=c(0, 100, 1000, Inf))) %>%
  select(-juiz, -reqdo_limpo, -qtd_agencias, -conciliacao, -re_collor, -n_partes) %>%
  mutate(audiencia=re_audiencia, hipossuf=re_hipossuf, serasa=re_inscr_indevida) %>%
  select(-reqdo_tem_adv, -hipossuf) %>%
  filter(resultado != 'outro') %>%
  mutate(valor_sentenca_cat=cut(valor_sentenca, breaks=c(0, 1000, 5000, Inf)))


# d_cat <- d_inadimpl %>% na.omit %>% select(-n_processo) %>%
#   mutate_each(funs(factor), -tempo, -valor_sentenca, -valor_acao)
#
# cats <- d_cat %>% apply(2, function(x) nlevels(as.factor(x)))
# mca1 <- d_cat %>% MCA(graph = FALSE)
# mca1_vars_df <- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
# mca1_obs_df <- data.frame(mca1$ind$coord)
# ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
#   geom_hline(yintercept = 0, colour = "gray70") +
#   geom_vline(xintercept = 0, colour = "gray70") +
#   geom_point(colour = "gray50", alpha = 0.1) +
#   geom_density2d(colour = "gray80") +
#   geom_text(data=mca1_vars_df,
#             aes(x= Dim.1, y=Dim.2, label=rownames(mca1_vars_df), colour=Variable)) +
#   scale_colour_discrete(name = "Variable") +
#   theme_bw()

d_reg <- d_inadimpl %>%
  na.omit %>%
  select(-n_processo) %>%
  mutate_each(funs(factor), -tempo, -valor_sentenca, -valor_acao) %>%
  select(-starts_with('re_'), -tempo, -valor_acao, -valor_sentenca) %>%
  data.frame