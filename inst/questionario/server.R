library(shiny)
library(dplyr)

load('populacao.RData')
path <- '../../data/txt'

n_processo <- function() {
  n_processo <- populacao %>% sample_n(1) %>% with(n_processo)
  n_processo
}

shinyServer(function(input, output, session) {
  
  observe({
    aux <- input$salvar
    
    n_processo_iso <- isolate({
      input$n_processo
    })
    
    if(!is.null(n_processo_iso)) {
      if(n_processo_iso != '') {
        isolate({
          dados <- data_frame(
            n_processo = n_processo_iso,
            onus_prova = 'onus_prova' %in% input$questoes,
            direito_consumidor = 'direito_consumidor' %in% input$questoes,
            pedido_dano_moral = 'pedido_dano_moral' %in% input$questoes,
            pedido_dano_material = 'pedido_dano_material' %in% input$questoes,
            plano_collor = 'plano_collor' %in% input$questoes,
            hipossuficiencia = 'hipossuficiencia' %in% input$questoes,
            pedido_gratuidade = 'pedido_gratuidade' %in% input$questoes,
            tentativa_conciliacao = 'tentativa_conciliacao' %in% input$questoes,
            abalo_aborr = 'abalo_aborr' %in% input$questoes,
            honorarios = 'honorarios' %in% input$questoes,
            plano_saude = 'plano_saude' %in% input$questoes,
            cobranca_indevida = 'cobranca_indevida' %in% input$questoes,
            reu_pj = 'reu_pj' %in% input$questoes,
            autor_pj = 'autor_pj' %in% input$questoes,
            serasa = 'serasa' %in% input$questoes,
            reu_banco = 'reu_banco' %in% input$questoes,
            reu_telefonia_internet = 'reu_telefonia_internet' %in% input$questoes,
            reu_hospital = 'reu_hospital' %in% input$questoes,
            reu_seguro = 'reu_seguro' %in% input$questoes,
            reu_financeira = 'reu_financeira' %in% input$questoes,
            relatorio_dispensado = 'relatorio_dispensado' %in% input$questoes,
            embargo_declaracao = 'embargo_declaracao' %in% input$questoes,
            termo_audiencia = 'termo_audiencia' %in% input$questoes,
            revelia = 'revelia' %in% input$questoes,
            resultado_dm = input$resultado_dm,
            resultado_dp = input$resultado_dp,
            valor_pedido = input$valor_pedido,
            valor_pago = input$valor_pago
          )
          saveRDS(dados, sprintf('salvos/%s.rds', n_processo_iso))
        })
      }
      n_processo_new <- n_processo()
      updateTextInput(session, 'n_processo', value = n_processo_new)
    }
  })
  
  output$sentenca <- renderUI({
    if(input$n_processo != '') {
      linhas <- readLines(sprintf('%s/%s.txt', path, input$n_processo))
      HTML(paste(linhas, collapse='<br/>'))
    }
  })
  
})

# sprintf("%s = '%s' in input$questoes", x, x) %>% str_replace_all(' in ', ' %in% ') %>% cat(sep=',\n')
