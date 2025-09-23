<h1 align="center">Medidor Orçamentista</h1>
<p align="center">
  <img src="https://img.shields.io/badge/LISP-990000?logo=commonlisp&logoColor=white&style=for-the-badge" />
  <img src="https://img.shields.io/badge/DCL-990000?logo=dcl&logoColor=white&style=for-the-badge" />
</p>
 
<p align="center">Este módulo permite medir automaticamente comprimentos, áreas, volumes e contagens de objetos no AutoCAD, atribuindo-os a layers específicos, alterando cores e exportando os resultados para CSV para orçamentação e controlo de obra.</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/OCipriano/medidor_orcamentista/refs/heads/main/Screenshot%202025-09-04%20170540.png" />
</p>

---

## 🚀 Funcionalidades

- Medição de objetos por:
  - Comprimento (ml)
  - Área (m²)
  - Volume (m³)
  - Contagem de unidades (un)
- Aplicação automática de **cores e layers** de acordo com a unidade de medição.
- Exportação de resultados para **CSV** com cabeçalho e escapamento de aspas em `medicoes.csv`.
- Fator de ajuste para resultados finais.
- Log de erros detalhado em `medicoes_log.txt`.
- Interface gráfica (DCL) amigável com botões de **Calcular**, **Limpar** e **Fechar**.

---

## 🛠️ Instalação

1. Copiar os ficheiros para a pasta de suporte do AutoCAD (Support Path):  

	- `medidor_orcamentista.lsp`
	- `medidor_orcamentista.dcl`

2. No AutoCAD, carregar o LISP usando o comando:

	```autocad
	APPLOAD
	```
	
	- Selecionar `medidor_orcamentista.lsp`.  

3. Ao carregar, deverá aparecer a mensagem:

	- Módulo Medidor Orçamentista - v1.0 carregado com sucesso. Use ´MEDORC´ para abrir o painel.

---

## 📋 Fluxo de Uso

1. Executar o comando no AutoCAD:

	```autocad
	MEDORC
	```

2. Selecionar o **elemento** a medir (ex: Paredes, Pavimentos).  
3. Selecionar a **unidade** (ml, m², m³, un).  
4. Inserir **código**, **descrição** e **fator de ajuste** se necessário.  
5. Selecionar objetos no desenho.  
6. Ver os resultados na **linha de comando** e verificar que foram exportados para CSV.  
7. Usar os botões do DCL:
	- **Calcular**: realiza a medição e exporta resultados.
	- **Limpar**: reinicia campos do diálogo.
	- **Fechar**: fecha o diálogo.

---

## 📁 Estrutura do Código

- `medidor_orcamentista.lsp` → Código principal do módulo.  
- `medidor_orcamentista.dcl` → Interface gráfica do diálogo.  
- `medicoes.csv`             → Ficheiro gerado com os resultados das medições.  
- `medicoes_log.txt`         → Log de erros e operações realizadas.

---

## ℹ️ Observações

- As layers de medição são criadas automaticamente caso não existam:
	- `Medido-comprimentos`
	- `Medido-areas`
	- `Medido-volumes`
	- `Medido-objectos`
- Se não forem selecionados objetos, a medição é abortada sem travar o AutoCAD.
- Os valores de volume exigem a introdução da altura/espessura do objeto.
- Todos os comentários e mensagens estão em **Português de Portugal**.
- Suporta seleção múltipla de objetos.

---

## 🔧 Requisitos

- AutoCAD versão **2010 ou superior** com suporte a AutoLISP e DCL.
- Sistema operativo **Windows**.
- Permissões para criar e gravar ficheiros (`medicoes.csv`, `medicoes_log.txt`).  

---

## 🧪 Testado em

- AutoCAD 2020 e superiores.
- Ambiente Windows 10 / 11.  

---

## 📬 Contribuição

Contribuições são bem-vindas!  
Se encontrares bugs ou tiveres sugestões, abre um **issue** ou faz um pull request.

---

## 📬 Contato

- Desenvolvido por **Mikey**  
- **Email:** code.wish815@passmail.com
- **GitHub:** [https://github.com/NunchuckCoder](https://github.com/NunchuckCoder)

---

## 🛡️ Licença

MIT License – Sinta-se à vontade para usar, modificar e distribuir o módulo com referência ao autor.
