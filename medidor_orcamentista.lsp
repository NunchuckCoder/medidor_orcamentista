;;----------------------=={ medidor_orcamentista.lsp }==-----------------------;;
;;                                                                             ;;
;;  Programa desenvolvido para auxiliar no processo de medi��es e              ;;
;;  or�amenta��o dentro do AutoCAD.                                            ;;
;;                                                                             ;;
;;  O sistema permite:                                                         ;;
;;   - Selecionar objetos e calcular automaticamente comprimentos (ml),        ;;
;;     �reas (m�), volumes (m�), unidades (un) ou pesos (kg).                  ;;
;;   - Aplicar um fator multiplicador a cada medi��o.                          ;;
;;   - Atribuir layer e cor aos objetos medidos, conforme o tipo de unidade.   ;;
;;   - Exportar os resultados para ficheiro CSV, incluindo elemento, c�digo,   ;;
;;     descri��o, unidade, quantidade, fator e total final.                    ;;
;;   - Registar erros em ficheiro de log (`medicoes_log.txt`) sem interromper  ;;
;;     a execu��o do programa.                                                 ;;
;;                                                                             ;;
;;  O utilizador pode escolher o elemento, unidade, c�digo e descri��o         ;;
;;  atrav�s de um painel DCL intuitivo, onde tamb�m pode calcular, limpar      ;;
;;  ou fechar o di�logo.                                                       ;;
;;                                                                             ;;
;;  NOTAS:                                                                     ;;
;;   - Para medi��es em m�, o programa solicitar� a altura/espessura.          ;;
;;   - Se n�o for selecionado nenhum objeto, a medi��o � anulada.              ;;
;;   - O ficheiro `medicoes.csv` � criado/atualizado automaticamente na        ;;
;;     pasta do desenho ativo.                                                 ;;
;;   - Caso o ambiente n�o suporte VLAX, o programa encerra e regista erro.    ;;
;;                                                                             ;;
;;-----------------------------------------------------------------------------;;
;;  FUN��O PRINCIPAL:  MEDORC                                                  ;;
;;-----------------------------------------------------------------------------;;
;;  Como usar:                                                                 ;;
;;   1. Carregar o LISP.                                                       ;;
;;   2. Executar o comando MEDORC.                                             ;;
;;   3. Preencher os campos no painel e selecionar os objetos a medir.         ;;
;;   4. O resultado ser� mostrado no AutoCAD e gravado em CSV.                 ;;
;;                                                                             ;;
;;-----------------------------------------------------------------------------;;
;;  Autor:   Osvaldo Cipriano                                                  ;;
;;  Vers�o:  1.0                                                               ;;
;;  Data:    Setembro 2025                                                     ;;
;;-----------------------------------------------------------------------------;;



;;-----------------------------------------------------------------------------;;
;;                      In�cio do carregamento do m�dulo                       ;;
;;-----------------------------------------------------------------------------;;

(princ "\nIniciando carregamento de medidor_orcamentista.lsp...")

;;-----------------------------------------------------------------------------;;
;;                            Fun��o de log de erros                           ;;
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
;;                             Fun��es utilit�rias                             ;;
;;-----------------------------------------------------------------------------;;

(defun EnsureLayer (layername /)
  "Garante que uma layer existe, criando se necess�rio."
  (if (not (tblsearch "layer" layername))
    (command "_.LAYER" "M" layername "")
  )
  layername
)

(defun CalcLength (obj / len)
  "Calcula comprimento de curva com seguran�a."
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
  "Calcula �rea de objeto com seguran�a."
  (vl-catch-all-apply 'vla-get-Area (list obj))
)

(defun EscapeCSV (str / s)
  "Escapa aspas e coloca o texto entre aspas para CSV."
  (setq s (vl-string-subst "\"\"" "\"" str)) ; substitui aspas internas por dupla aspas
  (strcat "\"" s "\"")
)

(defun ProcessSelection (ss unidade layer color altura / i ent obj val)
  "Processa sele��o de objetos: calcula total, muda cor e layer.
   - ss: sele��o de objetos
   - unidade: 'ml', 'm�', 'm�', 'un'
   - layer: layer onde colocar o objeto
   - color: cor do objeto
   - altura: para volume (m�)"
  (setq val 0.0)
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq obj (vlax-ename->vla-object ent))
	
    ;; --={  Calcula total conforme unidade  }=--
	
    (cond
      ((= unidade "ml")  (setq val (+ val (CalcLength obj))))
      ((= unidade "m�")  (setq val (+ val (CalcArea obj))))
      ((= unidade "m�")  (setq val (+ val (* (CalcArea obj) altura))))
      ((= unidade "un")  (setq val (1+ val)))
      (T (prompt "\nUnidade n�o suportada para c�lculo"))
    )
	
    ;; --={  Ajusta cor e layer do objeto  }=--
	
    (vla-put-color obj color)
    (vla-put-layer obj layer)
  )
  val
)

;;-----------------------------------------------------------------------------;;
;;              Inicializa��o VLAX  e verifica��o de depend�ncias              ;;
;;-----------------------------------------------------------------------------;;

(prompt "\nVerificando suporte a VLAX...")
(if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-get-acad-object '()))
  (progn
    (prompt "\nVLAX n�o suportado. Encerrando...")
    (LogError "[Erro] Este ambiente n�o suporta VLAX. O programa n�o pode ser executado.")
    (exit)
  )
  (progn
    (prompt "\nCarregando VLAX...")
    (vl-load-com)
    (prompt "\nVLAX carregado com sucesso.")
    (prompt "\nM�dulo Medidor Or�amentista - v1.0 carregado com sucesso. Use MEDORC para abrir o painel.")
  )
)

;;-----------------------------------------------------------------------------;;
;;                           Fun��o principal MEDORC                           ;;
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
  
  (setq elementos_list '("Demoli��es" "Movimento Terras" "Funda��es" "Bet�o" "Alvenarias"
                         "Coberturas" "Cantarias" "Carpintarias" "Serralharias"
                         "Pavimentos" "Paredes" "Tectos" "Pinturas" "Diversos"))
  (setq unidades_list '("ml" "m�" "m�" "un" "kg"))

  ;; --------------------------------------------------------------------------;;
  ;;                       Preencher popup_lists do DCL                        ;;
  ;; --------------------------------------------------------------------------;;
  
  (start_list "elemento") (mapcar 'add_list elementos_list) (end_list)
  (start_list "unidade")  (mapcar 'add_list unidades_list)  (end_list)

  ;; --------------------------------------------------------------------------;;
  ;;                    Definir valores iniciais do di�logo                    ;;
  ;; --------------------------------------------------------------------------;;
  
  (set_tile "fator" "1.0")        ; fator por defeito
  (set_tile "selecionar" "1")     ; selecionar objetos por defeito
  (set_tile "elemento" "0")       ; primeiro elemento da lista
  (set_tile "unidade" "0")        ; primeira unidade da lista

  ;; --------------------------------------------------------------------------;;
  ;;                             Bot�es do di�logo                             ;;
  ;; --------------------------------------------------------------------------;;
  
  (action_tile "fechar" "(done_dialog 0)")
  
  ;; --={  Bot�o Calcular  }=--
  
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
  
  ;; --={  Bot�o Limpar  }=--
  
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
  ;;                             Executar di�logo                              ;;
  ;; --------------------------------------------------------------------------;;
  
  (setq opcao (start_dialog))
  (unload_dialog dcl_id)

  ;; --------------------------------------------------------------------------;;
  ;;                         Processar op��o escolhida                         ;;
  ;; --------------------------------------------------------------------------;;
  
  (cond
  
    ;; --={  Calcular  }=--
	
    ((= opcao 1)
	
     ;; --={  Obter valores do di�logo  }=--
	 
	 (setq elemento (nth (atoi elemento_idx) elementos_list))
     (setq unidade  (nth (atoi unidade_idx)  unidades_list))
	 
	 ;; --={  Garante que o fator m�nimo seja 1.0  }=--
	 
     (if (or (not fator) (<= fator 0.0)) (setq fator 1.0))
	 
	 ;; --={  Inicializa a vari�vel total antes de calcular a medi��o  }=--
	 
     (setq total 0.0)

     ;; -----------------------------------------------------------------------;;
     ;;                           Sele��o de objetos                           ;;
     ;; -----------------------------------------------------------------------;;
	 
	 (if (= (atoi sel) 1)
       (progn
         (prompt "\nSelecione objetos para medir:")
         (setq ss (ssget))
		 
		 ;; --={  Sai se n�o houver objetos selecionados  }=--
		 
         (if (not ss)
           (progn
             (prompt "\nNenhum objeto selecionado. Abortando medi��o.")
             (setq total 0.0) ; evita crash
           )

         ;; --={  Define layer e cor conforme unidade  }=--
		 
         (cond
           ((= unidade "ml")  (setq layer (EnsureLayer "Medido-comprimentos") color 171))
           ((= unidade "m�")  (setq layer (EnsureLayer "Medido-areas")        color 171))
           ((= unidade "m�")  (setq layer (EnsureLayer "Medido-volumes")      color 171))
           ((= unidade "un")  (setq layer (EnsureLayer "Medido-objectos")     color 171))
           (T (prompt "\nUnidade n�o suportada para c�lculo"))
         )

		 ;; --={  Para volume, pede altura  }=--
		 
         (if (= unidade "m�")
           (progn
             (setq altura (getreal "\nInforme a altura/espessura (m): "))
             (if (or (not altura) (<= altura 0.0)) (setq altura 1.0))
           )
         )

         ;; --={  Processa sele��o de objetos  }=--
		 
         (setq total (ProcessSelection ss unidade layer color altura))
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
     ;;                           Exporta��o para CSV                          ;;
     ;; -----------------------------------------------------------------------;;
	 
     (setq filename (strcat (getvar "DWGPREFIX") "medicoes.csv"))
     (setq newfile (or (not (findfile filename)) (= (vl-file-size filename) 0)))
     (setq file (open filename "a"))
     (if newfile (write-line "Elemento,C�digo,Descri��o,Unidade,Quantidade,Fator,Total" file))
     (write-line
       (strcat
         (EscapeCSV elemento) ","   ; Ex: "Paredes"
         (EscapeCSV codigo) ","     ; Ex: "12345"
         (EscapeCSV descricao) ","  ; Ex: "Fachada A"
         unidade ","                ; Ex: "m�"
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
	
    ((= opcao 0) (prompt "\nDi�logo fechado."))
  )

  (princ)
)

;;-----------------------------------------------------------------------------;;
;;                      Mensagem Final ao carregar o LISP                      ;;
;;-----------------------------------------------------------------------------;;

(prompt "\nCriado por Osvaldo Cipriano.\n")
(princ)
