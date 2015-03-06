library(shiny)

fluidPage(
  
  titlePanel('Classificacao de processos do TJSP'),
  
  sidebarLayout(
    sidebarPanel(
      textInput('n_processo', 'N do processo'), 
      
      checkboxGroupInput('questoes', 'Questoes', 
                         c('onus_prova',
                           'direito_consumidor',
                           'pedido_dano_moral',
                           'pedido_dano_material',
                           'plano_collor',
                           'hipossuficiencia',
                           'pedido_gratuidade',
                           'tentativa_conciliacao',
                           'abalo_aborr',
                           'honorarios',
                           'plano_saude',
                           'cobranca_indevida',
                           'reu_pj',
                           'autor_pj',
                           'serasa',
                           'reu_banco',
                           'reu_telefonia_internet',
                           'reu_hospital',
                           'reu_seguro',
                           'reu_financeira',
                           'relatorio_dispensado',
                           'embargo_declaracao',
                           'termo_audiencia',
                           'revelia'
                           )),
      radioButtons('resultado_dm', 'Resultado DM', c('total', 'parcial', 'negado', 'acordo', 'extinto', 'nao_aplicavel')),
      radioButtons('resultado_dp', 'Resultado DP', c('total', 'parcial', 'negado', 'acordo', 'extinto', 'nao_aplicavel')),
      numericInput('valor_pedido', 'Valor pedido', value = 0),
      numericInput('valor_pago', 'Valor pago', value = 0),
      
      actionButton('salvar', 'Salvar e proximo')
    ),
    mainPanel(
      uiOutput('sentenca')
    )
  )
  
)