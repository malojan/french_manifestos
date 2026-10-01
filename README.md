# French Manifesto Corpus

This repository compiles the electoral programmes of French political parties for
presidential, legislative and European elections, in PDF and plain-text form, and
splits them into sentences for text analysis. A version of the sentences is classified
into Comparative Agendas Project (CAP) policy issues.

## Repository structure

```
raw_data/
  PRELEG/<year>/      Presidential and legislative elections: 2012, 2017, 2022, 2024
  EUR/<year>/         European elections: 2019, 2024
data/
  manifestos/         Sentence-level files, one per election, plus a combined file
  manifestos-cap/     Combined sentences with CAP issue predictions
```

### Raw documents (`raw_data/`)

Each election folder contains the original documents (`pdf/`) and their extracted text
(`txt/`). `MAN` files are party manifestos; `PF` files are candidates' *professions de
foi* (election addresses). Only manifestos are used in the sentence files; the 2012
documents are included as raw files but not yet in the sentence files.

## Sentence files (`data/manifestos/`)

| File | Election | Parties | Sentences |
|---|---|---|---|
| `PRE2017_manifestos_sentences.csv` | Presidential 2017 | FN, LFI, LR, LREM, PS | 4,042 |
| `EURO2019_manifestos_sentences.csv` | European 2019 | EELV, LFI, LR, PCF, PS, REN, RN | 2,545 |
| `PRE2022_manifestos_sentences.csv` | Presidential 2022 | EELV, LFI, LO, LR, PCF, PS, REC, REN, RN | 6,558 |
| `EURO2024_manifestos_sentences.csv` | European 2024 | EELV, LFI, LR, PCF, PS, REC, REN, RN | 4,158 |
| `LEG2024_manifestos_sentences.csv` | Legislative 2024 | ENS, FP, RN | 632 |
| `all_manifestos_sentences.csv` | All of the above | | 17,935 |

Columns:

| Column | Description |
|---|---|
| `doc` | Document identifier, `<party>_<year>` |
| `sentence_id` | Sentence identifier, `<doc>_<n>` (order within the document) |
| `election` | `PRE` (presidential), `LEG` (legislative) or `EURO` (European) |
| `year` | Election year |
| `party` | Party abbreviation |
| `sentence` | One sentence of the programme |

## CAP-classified sentences (`data/manifestos-cap/`)

The [Comparative Agendas Project](https://www.comparativeagendas.net/) (CAP) is a
codebook for classifying political texts by policy topic, designed to be applied in the
same way across countries, institutions and time. Each text is assigned one major topic,
such as Macroeconomics, Health, Environment, Immigration or Defense, which makes it
possible to measure how much attention parties or governments give to each issue and to
compare it across actors and over time.

`manifestos_cap_classified_2017_2024.csv` contains the same 17,935 sentences as
`all_manifestos_sentences.csv`, with two added columns:

| Column | Description |
|---|---|
| `policy_cap_predicted` | Predicted CAP policy issue (20 categories, e.g. Environment, Immigration, Health) |
| `policy_cap_score` | Model confidence for the predicted issue |

Issues were predicted with `poltextlab/xlm-roberta-large-french-cap-v3`, a classifier
developed by [PolTextLab](https://poltextlab.com/): an XLM-RoBERTa-large language model
fine-tuned on French political texts manually coded with the CAP codebook. Each sentence
receives the single most likely major topic. The model is no longer publicly listed on
Hugging Face. As with any automated classification, some sentences are misclassified,
so topic shares are best interpreted in aggregate (by party, election or issue) rather
than sentence by sentence.

## How the data were produced

For each election, the manifesto text files in `raw_data/` were combined with election,
year and party information, split into sentences with `tidytext::unnest_tokens(token =
"sentences")` in R, and numbered within each document.
