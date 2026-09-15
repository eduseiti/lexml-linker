# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

LexML Linker: a Haskell tool that finds references ("remissões") to Brazilian legislation in Portuguese text or HTML/XML (e.g. "art. 8º da Lei nº 12.527, de 18 de novembro de 2011") and turns them into LexML URNs (`urn:lex:br:federal:lei:2011-11-18;12527!art8`). Identifiers, comments, commit messages and CLI help are in Portuguese; keep that convention.

## Build and run

The package is `lexml-parser.cabal` (two executables, no library, **no test suite**). Build needs `alex` 3.2.x on PATH, because `src/main/haskell/LexML/Linker/Lexer.x` is compiled to `Lexer.hs`. The generated file is gitignored, so edit the `.x` file, not the generated one.

```bash
cabal v2-build all                      # or: make  (runs cabal new-build)
stack build                             # alternative; stack.yaml pins lts-20.26
cabal v2-run linkertool -- -f 'Os incisos I e III do art. 8o da Lei n.o 12.527, de 18 de novembro de 2011'
echo '<p>... art. 5º da Lei 8.666/1993 ...</p>' | cabal v2-run linkertool -- --hxml --xml
./build.sh                              # Docker image lexmlbr/lexml-linker:<cabal version>
```

- `build.sh` reads the file `version`, which sets `HASKELL_VERSION` (currently GHC 9.4.7), `LEXML_ALPINE_GLIBC_VERSION` and the image tag (taken from the `version:` field in the cabal file). The image installs `/usr/bin/linkertool` and `/usr/bin/simplelinker`.
- `packages/` can hold a pre-downloaded Hackage index, copied into the image so the Docker build can skip `cabal update`.
- A release is a bump of `version:` in `lexml-parser.cabal`. `LexML/Version.hs` and `src/main/bash/makeversion.sh` are stale SVN-era leftovers.

Since there are no tests, check a change by running `linkertool` on sample phrases. To debug a rule, add `--logaregras` (rule trace), `--logatokens` (lexer output) and `-d` (enables DEBUG logging, which is where those traces go). Without `-d` the traces are not printed. `linkertool` also drops linker errors without printing anything (`Left err -> return ()`), so empty output can mean "no match" or "error".

## Executables

- `linkertool` (`LinkerTool.hs`): the main CLI, built with `cmdargs`. Its flags are documented in README.md. Input is `--text` (default) or `--hxml`. Output is `--urns` (default, a sorted unique list), `--html` (`<a href>` built from the `--enderecoresolver` template, with `URNLEXML` replaced by the URN) or `--xml` (`<span xlink:href>`). `--contexto` accepts `federal`, `senado`, a full URN, or `INLINE`. With `INLINE`, stdin is a stream of blocks: a line with the context URN, then the text, then a line `###LEXML-END###`. Each output block is also terminated with `###LEXML-END###`. External callers use this for batch processing.
- `simplelinker` (`SimpleLinker.hs`): reads line-by-line from stdin with a fixed federal context, tagged input and XLink output.

The cabal file lists `other-modules` separately for each executable, so **a new module must be added to both lists**. `LexML.Cache` and `LexML.ServerMode` are leftovers from the removed HTTP server (only `linkertool` compiles them, and they're unused). `MakeMunicipios.hs` and `LexML/URN/Samples.hs` are not compiled at all.

## Pipeline (`LexML.Linker.linker`)

1. **Tags**: TagSoup parses the input (`IT_TAGGED`), or wraps it as one `TagText` (`IT_TEXT`). `HtmlCleaner` removes empty anchors.
2. **Lexing** (`Lexer.x` + `LexerPrim.hs`): runs over only the `TagText` nodes and produces `Token = ((tagIndex, charOffset), TokenData)`. `TokenData` includes `Palavra`, `Numero`, `Ordinal`, `IndicadorOrdinal` (º/ª/°), `Paragrafo`/`Paragrafos` (§/§§), punctuation and so on. Positions refer to tag index plus offset, which is how matches spanning markup get decorated later.
3. **Parsing** (`Parser.parseReferencias2`): Parsec over the token list, in the monad `LinkerParserMonad = ParsecT [Token] () (ExceptT LinkerParseError (State LinkerParserState))`. The top level is `many (choice (ignored : map (try . parseCase2 ctx) (pSTF : Regras2.parseCases) ++ [skip]))`, meaning it tries every rule at each token and skips one token on failure.
4. **Rules** return `ParseCase2 = [(startPos, endPos, URNLexML -> URNLexML)]`. `parseCase2` applies each function to the **context URN**, so rules are URN transformers: they refine or replace the context and don't build URNs from scratch. `addurn` records the result in a `DecorationMap` (`Decorator.hs`).
5. **Output**: either the URNs from the map, or `Decorator.decorate` splices open/close tags into the tag stream at the recorded positions, followed by optional `URLTransform.rewriteAnchors` and then `Render.render`.

## Where rules live

- `LexML/Linker/Regras2.hs` holds almost all recognition logic, and most feature commits touch only this file.
  - `parseCases` → `checkInitialToken >> norma`. `checkInitialToken` rejects the position quickly unless the token is in `initialWords`, so **a rule that starts with a new word also needs that word added to `initialWords`**.
  - `norma = (artigo/parágrafo/... component chain) `combineM` norma'`. Structural components are declared as `ComponenteParseInfo` records (`compArtigo` → `compParagrafo` → `compInciso` → `compAlinea` → `compItem`). Each record gives singular/plural names, abbreviations, numbering parsers (arabic, roman, ordinal, alphabetic), a `selectComp` fragment selector, and a sub-component, and generic combinators (`parseComponente*`) interpret them.
  - `norma'` covers the constitution (`constituicao1988`, only when `lpsConstituicaoSimples` is set), named documents (`listaApelidosSimples`: regimentos, ADCT, etc.) and `normaExtenso`: `tipoNorma` (lei, decreto, resolução, emenda constitucional, medida provisória) plus qualifiers (number, date, município, estado, autoridade), merged by `consolidaQualificadores`.
- `LexML/Linker/RegrasSTF.hs` handles STF-style coded references such as `LEG-FED LEI-008666 ANO-1993`.
- `Municipios.hs` / `Estados.hs` are large lookup tables for place-name qualifiers.
- `LexML/URN/`: `Types.hs` is the URN ADT, `Show.hs` serializes it (`urnShow`), `Parser.hs` parses URN strings (used for `--contexto`), `Atalhos.hs` has URN builders/selectors (`selecionaArtigo`, `selecionaNorma`, `apelido*`, ...), and `Utils.hs` has helpers such as `autoridadeConvencionada`, which maps an institution to federal/state/etc.

### Context-sensitive behaviour

The context URN (`lpsContexto`, available through `getUrnContexto`) changes what gets recognized. Behaviour changes often come from these checks, not from the grammar:
- `contextoResolucao`: "resolução" and bare "regimento interno/comum" are recognized only when the context document type is `resolucao` or `projeto resolucao`, and `autoridadeEm` further restricts by institution.
- `normaExtenso`: types flagged `precisaAutoConv` take the esfera (federal/estadual/…) from the context authority via `URN/Utils.autoridadeConvencionada`. To make a new institution count as federal, add it there.
- `loConstituicaoSimples` is set from `refContextoFederal`: a bare "Constituição" means CF/1988 only in a federal context.
- `prev_tokens` (`lpsPrevTokens`) lets a rule inspect already-consumed tokens. For example, `constituicao1988` refuses a match right after "de".
