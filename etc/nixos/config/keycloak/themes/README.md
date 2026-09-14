Você já possui o **`Username Password Form`** pronto dentro do subfluxo `browser single page login forms` (segunda imagem)! 

O que está fazendo a tela dividir é o subfluxo que está rodando antes dele: o **`browser single page login Organization`** (primeira imagem).

---

### O que fazer:

#### 1. O que remover:
* Na linha **`flow browser single page login Organization`** (a 4ª linha da primeira imagem), clique no **ícone de lixeira 🗑️** na ponta direita para **excluir esse subfluxo inteiro**.
  *(Ao remover ele, o `Organization Identity-First Login` e a condição vinculada a ele serão removidos juntos).*

#### 2. O que adicionar:
* **Nada!** Você não precisa adicionar nada novo.
* Como mostrado na segunda imagem, você já tem o subfluxo `browser single page login forms` com o **`Step Username Password Form`** (como `Required`). Sem o subfluxo de Organization na frente, o Keycloak chamará diretamente este formulário, renderizando a sua tela [login.ftl](file:///etc/nixos/config/keycloak/themes/themes/portal/login/login.ftl) com usuário e senha juntos.

---

### Passo final (para ativar):
Se você ainda não vinculou este fluxo duplicado:
1. No canto superior direito da página (ou no menu de 3 pontinhos do fluxo), clique em **Action** > **Bind flow**.
2. Escolha **Browser flow**.
