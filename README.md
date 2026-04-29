# Trabalho de Autômatos

Implementação em Haskell para conversão de autômatos e geração de AFNε a partir de expressões regulares.

## Funcionalidades

- Conversão de AFNε para AFN
- Conversão de AFN para AFD
- Geração de AFNε a partir de expressão regular usando Construção de Thompson

## Ambiente

O projeto inclui `flake.nix` para criar o ambiente com Nix.

Para entrar no ambiente:

```bash
nix develop
```

Para executar:

```bash
cabal run
```

## Uso

Ao executar, o programa mostra o menu:

```text
1 - Converter automato YAML
2 - Converter regex para NFAe
```

## Opção 1: Conversão de autômato

A entrada deve estar no arquivo `entrada.yaml`.

O programa usa o campo `type` para decidir a conversão:

- `nfae` gera um `nfa`
- `nfa` gera um `dfa`

A saída é escrita em `output.yaml`.

## Opção 2: Expressão regular

A expressão regular é digitada no terminal.

Exemplo:

```text
a(b|c)*
```

A saída é um AFNε escrito em `output.yaml`.

