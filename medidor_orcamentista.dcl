//--------------------=={ medidor_orcamentista.dcl }==--------------------//
//                                                                        //
//  Defini��o do di�logo gr�fico para o programa                          //
//  "Medidor Or�amentista - v1.0".                                        //
//                                                                        //
//  Este painel fornece ao utilizador uma interface intuitiva para:       //
//   - Selecionar o elemento da medi��o a partir de uma lista.            //
//   - Introduzir c�digo e descri��o do item a medir.                     //
//   - Escolher a unidade de medi��o (ml, m�, m�, un, kg).                //
//   - Definir um fator multiplicador.                                    //
//   - Ativar/desativar a sele��o de objetos no desenho.                  //
//                                                                        //
//  O painel cont�m tr�s bot�es principais:                               //
//   - [Calcular] : processa a sele��o e exporta os resultados.           //
//   - [Limpar]   : reinicia todos os campos do formul�rio.               //
//   - [Fechar]   : encerra o di�logo sem efetuar c�lculos.               //
//                                                                        //
//  NOTAS:                                                                //
//   - O di�logo � chamado pela fun��o MEDORC definida em                 //
//     "medidor_orcamentista.lsp".                                        //
//   - As listas de "Elemento" e "Unidade" s�o preenchidas dinamicamente  //
//     pelo c�digo LISP.                                                  //
//                                                                        //
//------------------------------------------------------------------------//
//  Autor:   Osvaldo Cipriano                                             //
//  Vers�o:  1.0                                                          //
//  Data:    Setembro 2025                                                //
//------------------------------------------------------------------------//

medidor_orcamentista : dialog {
    label = "Medidor Or�amentista - v1.0";
    : row {
        : column {
            : text { label = "Elemento:"; }
            : popup_list { key = "elemento"; }

            : text { label = "C�digo:"; }
            : edit_box { key = "codigo"; width = 20; }

            : text { label = "Descri��o:"; }
            : edit_box { key = "descricao"; width = 30; }
        }
        : column {
            : text { label = "Unidade:"; }
            : popup_list { key = "unidade"; }

            : text { label = "Fator multiplicador:"; }
            : edit_box { key = "fator"; width = 10; }
			
			: text { label = ""; }
            : toggle { key = "selecionar"; label = "Selecionar objetos no desenho"; }
        }
    }

    : row {
        : button { key = "calcular"; label = "Calcular"; }
        : button { key = "limpar"; label = "Limpar"; }
        : button { key = "fechar"; label = "Fechar"; is_cancel = true; }
    }
}
