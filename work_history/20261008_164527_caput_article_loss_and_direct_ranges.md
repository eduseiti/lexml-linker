# Article lost in "art. N, caput", direct-form ranges and related linker gaps

**Created:** 2026-10-08 16:45:27 (updated the same day with §3)
**Reported in:** `../lexml-parser-projeto-lei/work_history/20261008_001450_batch_parse_no_justificativa_only_and_tse_resolution.md`,
§3 ("Known issue: linker output for 'art. N, caput'").
**Changed:** `src/main/haskell/LexML/Linker/Regras2.hs`, `src/main/haskell/LexML/URN/Atalhos.hs`.

Status: **done** (2026-10-08). Not committed.

---

## 1. The reported case: stale binary, already fixed in the source

`O art. 19, caput, da Lei nº 9.096, de 19 de setembro de 1995` should give `…;9096!art19_cpt`. The
current source does give that, in every mode the parser uses (`--text`, and `--hxml --xml
--contexto=INLINE` with the document URN as context). A capture of the real stdin that
`lexml-parser-projeto-lei` sends for `lei_9504_19970930.docx` (a `tee` wrapper passed as `--linker`)
also links it correctly.

The `!cpt` output comes from the binary used on the other machine. Its `.err.log` points at
`/work/doutorado/…`, and its `/usr/local/bin/linkertool` dates from 2025-10-31. That binary predates
commit `5687f8d` ("Suporte à referência ao caput em referências inversas (#215565)", 2026-02-11).
That commit moved `caput` from `nomesCompSingular` to the new `nomesSemNumero` field of
`compParagrafo`. Before the commit, the direct form (`art. 19, caput`) failed and the inverse form
took over from "caput", applying `selecionaCaput` to the bare law.

I built `5687f8d^` and it reproduces the report exactly:

| Input | `5687f8d^` | HEAD |
|---|---|---|
| `O art. 19, caput, da Lei nº 9.096, de 19 de setembro de 1995` | `!cpt` | `!art19_cpt` |
| `arts. 46, caput, e 52, inciso I, do Decreto nº 70.235, de 1972` | `!cpt_inc1` | `!art46_cpt`, `!art52_cpt_inc1` |
| `art. 16, caput e § 4º da Lei nº 8.981, de 20 de janeiro de 1995` | `!cpt`, `!par4` | `!art16_cpt`, `!art16_par4` |

**Action for the other machine:** rebuild `linkertool` from this repo (or point
`/usr/local/bin/linkertool` at the new build). On this machine `/usr/local/bin/linkertool` is a
symlink to `dist-newstyle/…/linkertool`, so it already runs the current build.

## 2. Ranges in the direct form

`art. 4º, incisos IV a VI, da Lei nº 9.250, de 1995` gave `!cpt_inc4` / `!cpt_inc6` at HEAD:

1. `norma` first tries `parseComponenteDireto compArtigo` (artigo → subcomponents). Its
   `compSingularComNumero` read one number and had no "`a` <number>" range, unlike `intervalo` in
   the inverse-form parsers.
2. The direct parse stopped after "IV", `norma'` failed on "a", and the `try` dropped the whole
   match.
3. The top-level loop skipped tokens and matched again at "incisos IV a VI, da Lei" with the inverse
   form. With no article, `compParagrafo`'s `selectDefault` (`selecionaCaput`) was applied to the
   bare law, which gave `!cpt_inc4`.

**Fix:** `compSingularComNumero` accepts an optional `a <number>` after the number. The end uses the
same number parsers and must be **greater** than the start, which avoids matching "art. 5º a 3
dias". It returns both endpoints, matching `intervalo`: the end points only, with no expansion of
the range. No subcomponents are parsed after a range.

| Input | HEAD | Fixed |
|---|---|---|
| `art. 4º, incisos IV a VI, da Lei nº 9.250, de 1995` | `!cpt_inc4`, `!cpt_inc6` | `!art4_cpt_inc4`, `!art4_cpt_inc6` |
| `art. 4º, §§ 1º a 3º, da Lei nº 9.250, de 1995` | `!par1`, `!par3` | `!art4_par1`, `!art4_par3` |
| `arts. 4º a 6º da Lei nº 9.250, de 1995` | (bare law) | `!art4`, `!art6` |
| `arts. 58-D a 58-I da Lei nº 10.833, …` | (bare law) | `!art58-4`, `!art58-9` |
| `art. 5º a 3 dias da Lei nº 9.250, de 1995` | (bare law) | (bare law) |

## 3. Follow-up: the other gaps found in the corpus survey

The survey of br-taxqa-r found more article-less links (`!par…`, `!cpt…`), and links bound to the
wrong law. These were all fixed as well.

### 3.1 Law first, then the provisions (`normaPosposta`)

Receita Federal texts cite as `(Lei nº 8.685, de 1993, art. 1º, §§ 2º e 3º, Lei nº 8.981, de 1995,
art. 34, …)`. The grammar only read component → law, so articles after a law were lost, and a
trailing `§ 1º, e Lei nº 154` attached the `§` to the *next* law.

The new rule `normaPosposta` is tried first in `norma`. It reads a law (`normaCorpo`), an optional
description (`- Lei dos Partidos`, `– Regulamento do Imposto sobre a Renda`), an optional
parenthetical (`(Código Tributário Nacional)`, `(RIR/1999)`), then `,` and `art./arts./artigo(s)`,
then the component chain. Each component is applied to that law, and the first span starts at the
law, as `combine` does. It refuses:

- `Lei …, e art. N` and `Lei …, § N`: the word right after the comma must be an article name.
- `Lei nº 1, art. 5º da Lei nº 2`: the components may not be followed by `de/do/da <law>`.

`norma'` was split into `normaPreposicao` + `normaCorpo` for this. `constituicao1988` also accepts
`Constituição Federal de 1988`.

### 3.2 Alínea directly under the caput or a parágrafo

`art. 55, § 2º, alínea "c"` lost the article: `compParagrafo`'s subcomponent is `compInciso`, and
the direct form could not skip it. `compInciso` now has `selectDefault = Just id`, so the direct form
can go § → alínea. (`selectDefault` is otherwise only passed to `combineM'`, which ignores it.)

The LexML parser emits `artN_cpt_aliN` (823 ids in br-taxqa-r) and `artN_parN_aliN`, never
`artN_aliN`. So `selecionaAlinea` now makes sure a caput is present, the same way `selecionaInciso`
does. `alínea c do art. 55` changes from `art55_ali3` to `art55_cpt_ali3`.

### 3.3 Bare numerals and letters (`numerosSemNome`, `numerosComNome`)

- `art. 4º, IV, da Lei …` linked only the law. A new CPI field `numerosSemNome` lists the numbers
  accepted without the component name. For `compInciso` that is `numeroRomanoMaiusculo`, an uppercase
  roman numeral covering the whole word, so "VIDA" and "vi" are rejected. For `compAlinea` it is a
  quoted letter (`art. 150, VI, "a"`). `único` still needs the name.
- `alínea c` (unquoted) was rejected in the direct form. A new field `numerosComNome` lists the
  numbers accepted right after the name: any letter for `compAlinea`. In list continuations the
  letter "e" is refused (`numeroAlfabetoExcetoE`), so in `alíneas “a” a “c”, e § 1º` the "e" is the
  conjunction.
- In `alíneas a, b e c` the "a" was eaten as the feminine article. The article prefix in
  `parseComponenteDiretoSingular` (`prefixoSingular`) now backtracks when no component follows it.
- Curly quotes (`“a”`, `Simbolo '\8220'`/`'\8221'`) are accepted wherever `"a"` is.
- `compItem` got `numerosEspecificos`, so `alínea “b”, item 2` works in the direct form.

### 3.4 List combinators that dropped or crossed items

- **Empty element after "e"** (`parseComponentePlural2`, `listaDe`). `parseComponenteSingular` can
  return `[]` without consuming anything, so "e" was accepted with nothing after it. In `o inciso
  VII do art. 2º, e os arts. 51 e 53 da Lei …` the inverse parse then failed, and the fallback lost
  the inciso. An empty element after "e", or after a separator, now fails.
- **Separator with no element after it** (`listaDe`). `sepBy1 (try m) sep` fails with consumed input
  on `…, e art. 10`, which discarded the elements already read (`art. 6º, caput, e alínea “c”, art.
  8º, …`). It is now `many (try (parseSeparator >> m))`, which also accepts mixed `,`/`;` separators
  (`art. 3º, § 4º; art. 18-A, § 24, art. 18-B`). `parseListaNumeros` had the same `sepBy` problem
  (`incisos I, II, e III`).
- **Trailing separator** (`parseComponentePlural2`, `parseListaNumeros`). The list accepted any
  trailing `;`, so `inciso I, alínea “i”; Lei nº 13.105` bound to the next law. A trailing separator
  is now accepted only before "e" or "ambos/ambas/todos/todas". `parseComponenteSingular` also takes
  an optional separator before "todos/ambos" (`incisos XXXI e XXXIII, e §§ 4º a 6º, todos do art.
  39`).

### 3.5 Second follow-up: article lists, "§ 1", amending laws

These came from the cases §5 of the first version left open, and from regressions the corpus review
found while fixing them.

- **`art. 1º, 2º, 3º`** (singular "art." + list). `compArtigo` got `numerosSemNome =
  [numeroArtigoIsolado]`. A bare number can't start a match, because the first token must be in
  `initialWords` and the law-first rule requires "art" after the comma. So it only applies to list
  continuations (`art. 9º, 10 e 11`, `art. 43, 620 e 624`). `numeroArtigoIsolado` refuses a number
  followed by `.ddd`, so in `Leis nºs 7.713 … e 8.036` the "8" of the law number is not article 8.
- **`art. 78, § 1, Lei n. 8.112/1990`.** The bare "§ 1" was not the problem: `numeroOrdinal` already
  accepts a plain number. The cause was the `norma'` prefix (`optional $ do optional virgula; …;
  choice [de,do,da]`), which failed after consuming the comma when no preposition followed. `norma'`
  now accepts the full preposition or only a comma. The comma-only form is refused when the law is
  followed by its own `, art…`, because the citation is then law-first and the leftover components
  belong to the previous law (`…, art. 59, e § 1º, Decreto-Lei nº 2.454, de 1988, art. 1º`).
- **`art. 59, e § 1º` / `art. 6º, e parágrafo único`**: "e" is accepted after the comma before a
  subcomponent. The parent and the child are both linked, in separate spans (`art59`,
  `art59_par1`). Likewise `art. 6º, caput, e alínea “c”` gives `art6_cpt` and `art6_cpt_ali3`.
- **`§ 1º e 8º`** (no comma) are paragraphs: the new CPI flag `continuaComE` is set only for
  `compParagrafo`. With a comma (`arts. 2º, 3º, § 1º, e 7º`) the number is still an article.
  Subcomponents now attach to the last item (`§ 5º e 6º, inciso II` gives `par5`, `par6_inc2`).
- **Amending laws.** In `…, com redação dada pela Lei nº 13.097, de 2015, art. 37, e art. 15, inciso
  II`, what follows the amending article belongs to the *amended* law. When `normaPosposta` starts
  with an amendment phrase (`palavrasAlteracao`: dad-, incluíd-, acrescentad-, acrescid-,
  introduzid-, alterad-, renumerad- + `pela/pelo`), it takes a single article (`artigoUnico`). Those
  words were added to `initialWords`. Bare "pelo" is not enough: `aprovado pelo Decreto nº 9.580, …,
  art. 35, § 4º, inciso III` keeps the full chain. Note that `lpsPrevTokens` can't be used for this,
  because it lives in the `State` under Parsec and is never rolled back on backtracking.
- **`numeroRomano` read partial words**: `palavraI` did not require the whole word, so "incluídos"
  was inciso I and "Lei" was inciso L (50) (HEAD gave `8981!art78_cpt_inc50`). It now ends with `eof`.
- **Two-digit years in `dd/mm/yy`**: `de 22/12/88` gave `lei:0088-12-22`, and `/03` gave 1903. A
  new helper `anoQuatroDigitos` (00–30 → 20xx, else 19xx) is used by `parseDataAbrev` and
  `parseQualData2`, so `5.452/43` is still 1943.

### 3.6 Examples

| Input | HEAD | Fixed |
|---|---|---|
| `(Decreto-Lei nº 5.844, de 1943, art. 62, § 1º, e Lei nº 154, de 1947, art. 1º)` | `5844`, `154!par1` | `5844!art62_par1`, `154!art1` |
| `Decreto nº 3.000, de 1999 – Regulamento … (RIR/1999), art. 75, incisos I e III` | `3000!cpt_inc1`, `…inc3` | `3000!art75_cpt_inc1`, `…inc3` |
| `art. 55, § 2º, alínea c, da Lei nº 7.799, de 1989` | `!ali3`, `!par2` | `!art55_par2_ali3` |
| `art. 55, alínea c, da Lei nº 7.799, de 1989` | `!ali3` | `!art55_cpt_ali3` |
| `art. 4º, IV, V e VI, da Lei nº 9.250, de 1995` | (bare law) | `!art4_cpt_inc4`, `inc5`, `inc6` |
| `art. 150, VI, “a”, da Constituição Federal` | — | `!art150_cpt_inc6_ali1` |
| `os incisos VII a IX do § 1º do art. 2º, e os arts. 51 e 53 da Lei nº 10.833` | `!art2`, `!art51`, `!art53` | `!art2_par1_inc7`, `…inc9`, `!art51`, `!art53` |
| `inciso II e § 4º do art. 19 da Lei nº 10.522` | `!art19_par4_inc2` | `!art19_cpt_inc2`, `!art19_par4` |
| `Art. 105, inciso I, alínea "i"; Lei nº 13.105, …` | `13105!ali9`, `13105!cpt_inc1` | `13105` |
| `art. 1º, 2º, 3º da Lei nº 7.713, de 1988` | (bare law) | `!art1`, `!art2`, `!art3` |
| `(art. 78, § 1, Lei n. 8.112/1990)` | (bare law) | `!art78_par1` |
| `(Lei nº 9.249, de 1995, art. 6º, e parágrafo único)` | (bare law) | `!art6`, `!art6_par1u` |
| `Lei nº 10.833, …, § 16, com a redação dada pela Lei nº 13.097, de 2015, art. 37, e art. 15, inciso II` | `13097!art15_cpt_inc2` | `10833!art3_par16`, `13097!art37` |
| `art. 6º da Lei nº 7.713, de 22/12/88` | `lei:0088-12-22;7713!art6` | `lei:1988-12-22;7713!art6` |
| `art. 4º, VIDA da Lei …` / `art. 4º, vi, da Lei …` | (bare law) | (bare law) |

## 4. Regression check

All 20,548 distinct `<p>` texts of `../br-taxqa-r_v2.0/lexml` that mention a component were linked
with an unmodified HEAD build and with the final build (INLINE mode, context
`urn:lex:br:federal:lei:2000-01-01;1`).

- 8,119 paragraphs differ. Most of the change is the law-first rule: bare-law URNs become article
  URNs.
- Article-less URNs (a `!` fragment without `art`) drop from **1,227 to 30**.
- The second follow-up (§3.5) changes 83 paragraphs compared with the first follow-up: 17 only by
  the year fix, 29 by added or refined links, and 37 others. I reviewed all 83. The 37 are corrections:
  mostly links that bound the amended law's provisions to the amending law, which are removed, plus
  "Lei" read as inciso L. The only loss is `na redação dada pela Lei n.º 9.250, de 1995, art. 6º,
  inciso III`, which now gives `art6` without the inciso; that citation is ambiguous.
- I reviewed every paragraph where an article URN disappeared without a replacement for the same
  article, and every paragraph where an article-less or bare-law URN appeared (26 + 14). All of them
  are corrections: the old URN was bound to the wrong law, had the wrong hierarchy
  (`art19_par4_inc2`), or was a fragment without an article.
- Several intermediate versions caused regressions, which the review caught and fixed: "ambos/todos
  do art. …" after a separator, and the letter "e" read as an alínea. The final build has none left
  in the review.
- The `CLAUDE.md` sample phrase, the `--hxml --xml` README example, the INLINE stream mode with
  markup inside a citation, and `simplelinker` all give the expected output. The build prints only
  the existing `-Wtabs` warning.

## 5. Still not handled

- After an amending law, provisions that continue the amended law (`…pela Lei X, art. 61; e § 3º,
  inciso II`) are not linked, because the amended law is too far back to bind to safely.
- `Leis nºs 7.713, de 1988, … e 8.036, de 1990` (plural "Leis" with several numbers): the second
  law is not recognized.
- Inverse chains with a missing level (`inciso I, alínea “i”, da Lei …` with no article) still give
  sibling URNs (`cpt_inc1`, `cpt_ali9`) instead of one chain.
- Text cut in the middle of a citation (chunks starting with "alínea “g”; Decreto …") cannot be
  fixed in the linker.

## 6. Reproducing

```bash
export PATH=/root/.ghcup/bin:$PATH LANG=C.UTF-8 LC_ALL=C.UTF-8
cabal v2-build all
echo '(Lei nº 9.250, de 1995, art. 4º, incisos IV a VI, e Lei nº 8.981, de 1995, art. 34)' \
    | $(cabal list-bin linkertool)
printf 'urn:lex:br:federal:lei:1997-09-30;9504\n<p>O art. 19, caput, da Lei nº 9.096, de 19 de setembro de 1995</p>\n###LEXML-END###\n' \
    | $(cabal list-bin linkertool) --hxml --xml --contexto=INLINE
```

The br-taxqa-r_v2.0 documents need to be re-parsed with the rebuilt linker to pick up these fixes
(`batch_parse.py --linker /usr/local/bin/linkertool`, as in the referenced document).
