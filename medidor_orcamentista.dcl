//--------------------=={ medidor_orcamentista.dcl }==--------------------//
//                                                                        //
//  Definição do diálogo gráfico para o programa                          //
//  "Medidor Orçamentista - v1.0".                                        //
//                                                                        //
//  Este painel fornece ao utilizador uma interface intuitiva para:       //
//   - Selecionar o elemento da medição a partir de uma lista.            //
//   - Indicar código e descrição do item a medir.                        //
//   - Escolher a unidade de medição (ml, m², m³, un, kg).                //
//   - Definir um fator multiplicador.                                    //
//   - Indicar se pretende escolher objetos no desenho.                   //
//                                                                        //
//  O painel contém três botões principais:                               //
//   - [Calcular] : processa a seleção e exporta os resultados.           //
//   - [Limpar]   : reinicia todos os campos do formulário.               //
//   - [Fechar]   : encerra o diálogo sem efetuar cálculos.               //
//                                                                        //
//  NOTAS:                                                                //
//   - O diálogo é chamado pela função MEDORC definida em                 //
//     "medidor_orcamentista.lsp".                                        //
//   - As listas de "Elemento" e "Unidade" são preenchidas dinamicamente  //
//     pelo código LISP.                                                  //
//                                                                        //
//------------------------------------------------------------------------//
//  Autor:   NunchuckCoder                                                //
//  Versão:  1.0                                                          //
//  Data:    Setembro 2025                                                //
//------------------------------------------------------------------------//

medidor_orcamentista : dialog {
    label = "Medidor Orçamentista - v1.0";
    : row {
        : column {
            : text { label = "Elemento:"; }
            : popup_list { key = "elemento"; }

            : text { label = "Código:"; }
            : edit_box { key = "codigo"; width = 20; }

            : text { label = "Descrição:"; }
            : edit_box { key = "descricao"; width = 30; }
        }
        : column {
            : text { label = "Unidade:"; }
            : popup_list { key = "unidade"; }

            : text { label = "Fator multiplicador:"; }
            : edit_box { key = "fator"; width = 10; }
			
			: text { label = ""; }
            : toggle { key = "selecionar"; label = "Indicar objetos no desenho"; }
        }
    }

    : row {
        : button { key = "calcular"; label = "Calcular"; }
        : button { key = "limpar"; label = "Limpar"; }
        : button { key = "fechar"; label = "Fechar"; is_cancel = true; }
    }
}
