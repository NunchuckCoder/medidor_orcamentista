;;----------------------=={ medidor_orcamentista.lsp }==-----------------------;;
;;                                                                             ;;
;;  Programa desenvolvido para auxiliar no processo de medições e              ;;
;;  orçamentação dentro do AutoCAD.                                            ;;
;;                                                                             ;;
;;  O sistema permite:                                                         ;;
;;   - Selecionar objetos e calcular automaticamente comprimentos (ml),        ;;
;;     áreas (m²), volumes (m³), unidades (un) ou pesos (kg).                  ;;
;;   - Aplicar um fator multiplicador a cada medição.                          ;;
;;   - Atribuir layer e cor aos objetos medidos, conforme o tipo de unidade.   ;;
;;   - Exportar os resultados para ficheiro CSV, incluindo elemento, código,   ;;
;;     descrição, unidade, quantidade, fator e total final.                    ;;
;;   - Registar erros em ficheiro de log (`medicoes_log.txt`) sem interromper  ;;
;;     a execução do programa.                                                 ;;
;;                                                                             ;;
;;  O utilizador pode escolher o elemento, unidade, código e descrição         ;;
;;  através de um painel DCL intuitivo, onde também pode calcular, limpar      ;;
;;  ou fechar o diálogo.                                                       ;;
;;                                                                             ;;
;;  NOTAS:                                                                     ;;
;;   - Para medições em m³, o programa solicitará a altura/espessura.          ;;
;;   - Se não for selecionado nenhum objeto, a medição é anulada.              ;;
;;   - O ficheiro `medicoes.csv` é criado/atualizado automaticamente na        ;;
;;     pasta do desenho ativo.                                                 ;;
;;   - Caso o ambiente não suporte VLAX, o programa encerra e regista erro.    ;;
;;                                                                             ;;
;;-----------------------------------------------------------------------------;;
;;  FUNÇÃO PRINCIPAL:  MEDORC                                                  ;;
;;-----------------------------------------------------------------------------;;
;;  Como usar:                                                                 ;;
;;   1. Carregar o LISP.                                                       ;;
;;   2. Executar o comando MEDORC.                                             ;;
;;   3. Preencher os campos no painel e selecionar os objetos a medir.         ;;
;;   4. O resultado será mostrado no AutoCAD e gravado em CSV.                 ;;
;;                                                                             ;;
;;-----------------------------------------------------------------------------;;
;;  Autor:   Mikey aka NunchuckCoder                                                  ;;
;;  Versão:  1.0                                                               ;;
;;  Data:    Setembro 2025                                                     ;;
;;-----------------------------------------------------------------------------;;



;;-----------------------------------------------------------------------------;;
;;                      Início do carregamento do módulo                       ;;
;;-----------------------------------------------------------------------------;;

(princ "\nIniciando carregamento de medidor_orcamentista.lsp...")

;;-----------------------------------------------------------------------------;;
;;                            Função de log de erros                           ;;
;;-----------------------------------------------------------------------------;;

(defun LogError (msg / file timestamp)

  ;; --={  Tenta gravar erro no log sem interromper o programa  }=--
  ;; --={  Cria ou abre "medicoes_log.txt" e escreve timestamp + mensagem  }=--
  
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq timestamp (rtos (getvar "CDATE") 2 4))
        (setq file (open "medicoes_log.txt" "a"))
        (write-line (strcat timestamp ": " msg) file)
        (close file)
        (prompt (strcat "\n[LOG] " msg))  ; mostra log na linha de comando
      )
    )
    '()
  )
  (princ)
)

;;-----------------------------------------------------------------------------;;
;;                             Funções utilitárias                             ;;
;;-----------------------------------------------------------------------------;;

(defun EnsureLayer (layername /)
  "Garante que uma layer existe, criando se necessário."
  (if (not (tblsearch "layer" layername))
    (command "_.LAYER" "M" layername "")
  )
  layername
)

(defun CalcLength (obj / len)
  "Calcula comprimento de curva com segurança."
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq len (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
        len
      )
    )
    '()
  )
)

(defun CalcArea (obj / a)
  "Calcula área de objeto com segurança."
  (vl-catch-all-apply 'vla-get-Area (list obj))
)

(defun EscapeCSV (str / s)
  "Escapa aspas e coloca o texto entre aspas para CSV."
  (setq s (vl-string-subst "\"\"" "\"" str)) ; substitui aspas internas por dupla aspas
  (strcat "\"" s "\"")
)

(defun ProcessSelection (ss unidade layer color altura / i ent obj val)
  "Processa seleção de objetos: calcula total, muda cor e layer.
   - ss: seleção de objetos
   - unidade: 'ml', 'm²', 'm³', 'un'
   - layer: layer onde colocar o objeto
   - color: cor do objeto
   - altura: para volume (m³)"
  (setq val 0.0)
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq obj (vlax-ename->vla-object ent))
	
    ;; --={  Calcula total conforme unidade  }=--
	
    (cond
      ((= unidade "ml")  (setq val (+ val (CalcLength obj))))
      ((= unidade "m²")  (setq val (+ val (CalcArea obj))))
      ((= unidade "m³")  (setq val (+ val (* (CalcArea obj) altura))))
      ((= unidade "un")  (setq val (1+ val)))
      (T (prompt "\nUnidade não suportada para cálculo"))
    )
	
    ;; --={  Ajusta cor e layer do objeto  }=--
	
    (vla-put-color obj color)
    (vla-put-layer obj layer)
  )
  val
)

;;-----------------------------------------------------------------------------;;
;;              Inicialização VLAX  e verificação de dependências              ;;
;;-----------------------------------------------------------------------------;;

(prompt "\nVerificando suporte a VLAX...")
(if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-get-acad-object '()))
  (progn
    (prompt "\nVLAX não suportado. Encerrando...")
    (LogError "[Erro] Este ambiente não suporta VLAX. O programa não pode ser executado.")
    (exit)
  )
  (progn
    (prompt "\nCarregando VLAX...")
    (vl-load-com)
    (prompt "\nVLAX carregado com sucesso.")
    (prompt "\nMódulo Medidor Orçamentista - v1.0 carregado com sucesso. Use MEDORC para abrir o painel.")
  )
)

;;-----------------------------------------------------------------------------;;
;;                           Função principal MEDORC                           ;;
;;-----------------------------------------------------------------------------;;

(defun c:MEDORC (/ dcl_id elemento_idx elemento
                 unidade_idx unidade codigo descricao fator sel
                 ss total quantidade total_final opcao layer altura color)
  
  ;; --------------------------------------------------------------------------;;
  ;;                               Carregar DCL                                ;;
  ;; --------------------------------------------------------------------------;;
  
  (setq dcl_id (load_dialog "medidor_orcamentista.dcl"))
  (if (not (new_dialog "medidor_orcamentista" dcl_id))
    (exit)
  )

  ;; --------------------------------------------------------------------------;;
  ;;                      Listas de elementos e unidades                       ;;
  ;; --------------------------------------------------------------------------;;
  
  (setq elementos_list '("Demolições" "Movimento Terras" "Fundações" "Betão" "Alvenarias"
                         "Coberturas" "Cantarias" "Carpintarias" "Serralharias"
                         "Pavimentos" "Paredes" "Tectos" "Pinturas" "Diversos"))
  (setq unidades_list '("ml" "m²" "m³" "un" "kg"))

  ;; --------------------------------------------------------------------------;;
  ;;                       Preencher popup_lists do DCL                        ;;
  ;; --------------------------------------------------------------------------;;
  
  (start_list "elemento") (mapcar 'add_list elementos_list) (end_list)
  (start_list "unidade")  (mapcar 'add_list unidades_list)  (end_list)

  ;; --------------------------------------------------------------------------;;
  ;;                    Definir valores iniciais do diálogo                    ;;
  ;; --------------------------------------------------------------------------;;
  
  (set_tile "fator" "1.0")        ; fator por defeito
  (set_tile "selecionar" "1")     ; selecionar objetos por defeito
  (set_tile "elemento" "0")       ; primeiro elemento da lista
  (set_tile "unidade" "0")        ; primeira unidade da lista

  ;; --------------------------------------------------------------------------;;
  ;;                             Botões do diálogo                             ;;
  ;; --------------------------------------------------------------------------;;
  
  (action_tile "fechar" "(done_dialog 0)")
  
  ;; --={  Botão Calcular  }=--
  
  (action_tile "calcular"
    "(progn
       (setq elemento_idx (get_tile \"elemento\"))
       (setq unidade_idx (get_tile \"unidade\"))
       (setq codigo (get_tile \"codigo\"))
       (setq descricao (get_tile \"descricao\"))
       (setq fator (atof (get_tile \"fator\")))
       (setq sel (get_tile \"selecionar\"))
       (done_dialog 1)
    )"
  )
  
  ;; --={  Botão Limpar  }=--
  
  (action_tile "limpar"
    "(progn
       (set_tile \"codigo\" \"\")
       (set_tile \"descricao\" \"\")
       (set_tile \"fator\" \"1.0\")
       (set_tile \"selecionar\" \"1\")
       (set_tile \"elemento\" \"0\")
       (set_tile \"unidade\" \"0\")
     )"
  )

  ;; --------------------------------------------------------------------------;;
  ;;                             Executar diálogo                              ;;
  ;; --------------------------------------------------------------------------;;
  
  (setq opcao (start_dialog))
  (unload_dialog dcl_id)

  ;; --------------------------------------------------------------------------;;
  ;;                         Processar opção escolhida                         ;;
  ;; --------------------------------------------------------------------------;;
  
  (cond
  
    ;; --={  Calcular  }=--
	
    ((= opcao 1)
	
     ;; --={  Obter valores do diálogo  }=--
	 
	 (setq elemento (nth (atoi elemento_idx) elementos_list))
     (setq unidade  (nth (atoi unidade_idx)  unidades_list))
	 
	 ;; --={  Garante que o fator mínimo seja 1.0  }=--
	 
     (if (or (not fator) (<= fator 0.0)) (setq fator 1.0))
	 
	 ;; --={  Inicializa a variável total antes de calcular a medição  }=--
	 
     (setq total 0.0)

     ;; -----------------------------------------------------------------------;;
     ;;                           Seleção de objetos                           ;;
     ;; -----------------------------------------------------------------------;;
	 
	 (if (= (atoi sel) 1)
       (progn
         (prompt "\nSelecione objetos para medir:")
         (setq ss (ssget))
		 
		 ;; --={  Sai se não houver objetos selecionados  }=--
		 
         (if (not ss)
           (progn
             (prompt "\nNenhum objeto selecionado. Abortando medição.")
             (setq total 0.0) ; evita crash
           )
           (progn
		   
         ;; --={  Define layer e cor conforme unidade  }=--
		 
             (cond
               ((= unidade "ml")  (setq layer (EnsureLayer "Medido-comprimentos") color 171))
               ((= unidade "m²")  (setq layer (EnsureLayer "Medido-areas")        color 171))
               ((= unidade "m³")  (setq layer (EnsureLayer "Medido-volumes")      color 171))
               ((= unidade "un")  (setq layer (EnsureLayer "Medido-objectos")     color 171))
               (T (prompt "\nUnidade não suportada para cálculo"))
             )

		 ;; --={  Para volume, pede altura  }=--
		 
             (if (= unidade "m³")
               (progn
                 (setq altura (getreal "\nInforme a altura/espessura (m): "))
                 (if (or (not altura) (<= altura 0.0)) (setq altura 1.0))
               )
             )

         ;; --={  Processa seleção de objetos  }=--
		 
             (setq total (ProcessSelection ss unidade layer color altura))
           )
         )
       )
     )

     ;; -----------------------------------------------------------------------;;
     ;;                               Resultados                               ;;
     ;; -----------------------------------------------------------------------;;
	 
     (setq quantidade total)
     (setq total_final (* quantidade fator))
     (prompt (strcat "\nMedido (bruto) de " descricao ": " (rtos quantidade 2 2) " " unidade))
     (prompt (strcat "\nTotal (com fator " (rtos fator 2 2) "): " (rtos total_final 2 2) " " unidade))

     ;; -----------------------------------------------------------------------;;
     ;;                           Exportação para CSV                          ;;
     ;; -----------------------------------------------------------------------;;
	 
     (setq filename (strcat (getvar "DWGPREFIX") "medicoes.csv"))
     (setq newfile (or (not (findfile filename)) (= (vl-file-size filename) 0)))
     (setq file (open filename "a"))
     (if newfile (write-line "Elemento,Código,Descrição,Unidade,Quantidade,Fator,Total" file))
     (write-line
       (strcat
         (EscapeCSV elemento) ","   ; Ex: "Paredes"
         (EscapeCSV codigo) ","     ; Ex: "12345"
         (EscapeCSV descricao) ","  ; Ex: "Fachada A"
         unidade ","                ; Ex: "m²"
         (rtos quantidade 2 2) ","  ; Quantidade antes do fator
         (rtos fator 2 2) ","       ; Fator aplicado
         (rtos total_final 2 2)     ; Total final com fator
       )
       file
     )
     (close file)
     (prompt (strcat "\nDados exportados para: " filename))
    )

    ;; --={  Fechar  }=--
	
    ((= opcao 0) (prompt "\nDiálogo fechado."))
  )

  (princ)
)

;;-----------------------------------------------------------------------------;;
;;                      Mensagem Final ao carregar o LISP                      ;;
;;-----------------------------------------------------------------------------;;

(prompt "\nCriado por Mikey aka NunchuckCoder.\n")
(princ)

