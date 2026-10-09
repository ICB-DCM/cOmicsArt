# PROTOTYPE 06 -- MultiOmics shell (throwaway)

Answers ticket 06 (`.scratch/multi-omics/tickets/06-shell-prototype.md`): how the
`Upload | Datasets | MultiOmics` shell looks and behaves, with Datasets created
dynamically and torn down with `session$destroy()`. No real analysis. Not part of
the app; lives only on branch `prototype/06-shell` (based on `main`, never merged).
Delete once the decisions are folded into the plan (Phase 4 in
`.scratch/multi-omics/PLAN.md`).

## What to react to

Click through the variants (see below) and decide each point. The ticket closes
with these answers. Recommendation in brackets.

1. **Tab structure:** `nested`, `flat` or `picker`? (flat: one tab row less, reads
   as the pipeline Upload → Datasets → MultiOmics, 5 Datasets still fit)
   1. Paul: each dataset its own top level tab
   2. Lea: TBA
2. **Dataset name and omic type:** header strip or every sidebar? (header strip,
   plus a coloured dot and the name in the tab label)
   1. Paul: above module tabs or (if possible, slight change to background that is linked to the colored dot?)
   2. Lea: TBA
3. **How much Dataset colour:** `stripe` or `frame`? (stripe; frame competes with
   the module sidebar colours. Check that the red and magenta Dataset colours don't
   clash with the logo. ML has no module colour today and is grey here.)
   1. Paul: No strong opinion. Actually like the frame (similar to my suggestion in 2.)
   2. Lea: TBA
4. **A removed Dataset:** `gone` or `tombstone`? (gone + toast; a struck-through
   badge only where something still refers to it: MultiOmics results, Upload list)
   1. Paul: Toombstone creates to much clutter potentially. remove completely
   2. Lea: TBA
5. **Soft cap of 5:** block or warn? (warn with "Add anyway", as built)
   1. Paul: Warn seems good. With a "Don't show again" and a "Dont show again this session"
   2. Lea: TBA
6. **Confirm every removal?** (yes; it discards Selection, preprocessing and all
   analyses)
   1. Paul: Yes
   2. Lea: TBA
7. **Reuse after removal:** (internal id `ds_k` never reused; display name may be)
   1. Paul: Alow reuse and actually remove the dataset! Keep it in in the Upload with a small info on what was selected and
   optional note on WHY it was deleted.
   2. Lea: TBA
8. **Where "Remove" lives:** (header strip only; no "×" on the tab)
   1. Paul: Yes, but make sure it is visible
   2. Lea: TBA
9. **MultiOmics with fewer than 2 Datasets:** hidden or disabled with a hint?
   (hidden, as built)
   1. Paul: We can click into it but it will show a "Upload at least two datasets for this!"
   2. Lea: TBA

Needs **shiny >= 1.14.0** (`session$destroy()`). The repo's `program/renv.lock`
pins 1.8.1.1, so the commands install shiny into a separate throwaway library and
leave renv alone.

## Run (local R, verified with R 4.6.1 + shiny 1.14.0 on macOS)

From the repo root:

```sh
mkdir -p ~/.cache/comicsart-proto-06
R_LIBS=~/.cache/comicsart-proto-06 Rscript -e 'if (!requireNamespace("shiny", quietly = TRUE) || packageVersion("shiny") < "1.14.0") install.packages("shiny", lib = Sys.getenv("R_LIBS"), repos = "https://cloud.r-project.org")'
R_LIBS=~/.cache/comicsart-proto-06 Rscript -e 'shiny::runApp("prototypes/06-shell", port = 3939, launch.browser = TRUE)'
```

Then open <http://127.0.0.1:3939>. Click **Demo: add 3 Datasets** on the Upload tab
for a quick start.

## Run (Docker, same image as the app)

Verified 2026-10-09 with `pauljonasjost/comicsart:latest` (R 4.2.1, amd64) on Apple
Silicon. Shiny 1.14 and its updated dependencies are compiled into a throwaway
library inside the container on every start, which takes about 2 minutes under
emulation; the image itself is not changed. From the repo root:

```sh
docker run --rm --name proto06 --platform linux/amd64 -p 3939:3939 \
  -v "$PWD/prototypes/06-shell:/proto" --entrypoint R \
  pauljonasjost/comicsart:latest -e 'lib <- tempfile(); dir.create(lib); .libPaths(c(lib, .libPaths())); install.packages("shiny", lib = lib, repos = "https://cloud.r-project.org"); shiny::runApp("/proto", host = "0.0.0.0", port = 3939)'
```

Open <http://localhost:3939> once the log shows `Listening on http://0.0.0.0:3939`.
Stop it with `docker rm -f proto06`.

## Headless check

```sh
R_LIBS=~/.cache/comicsart-proto-06 Rscript prototypes/06-shell/check.R
```

`shiny::testServer` run: add (name validation, duplicate names, default name
`Transcriptomics_2`), 1 Dataset scope + 9 nested module scopes per Dataset, remove
calls `session$destroy()` (onDestroy runs for all 10 scopes, `ds_k-*` inputs gone,
destroyed observers no longer fire, other Datasets keep firing), soft-cap modal and
"Add anyway", ids never reused. (The `max(x)` warnings it prints come from plot
rendering in the mock session; the real app logs none.)

## Variants (bottom bar; switching reloads the page and loses state)

URL params: `?layout=nested|flat|picker&badge=header|sidebar&colour=stripe|frame&removed=gone|tombstone`

| param   | options |
|---------|---------|
| layout  | `nested`: Datasets tab with Dataset tabs inside, then module tabs. `flat`: every Dataset is a top-level tab between Upload and MultiOmics. `picker`: Datasets tab with a radio picker instead of tabs. |
| badge   | `header`: name + omic type + source strip above the module tabs. `sidebar`: badge at the top of every module sidebar. |
| colour  | `stripe`: Dataset colour only on dot, badge border and header stripe. `frame`: Dataset colour frames and lightly tints the whole Dataset area. |
| removed | `gone`: tab disappears, toast. `tombstone`: greyed, struck-through tab stays until closed. |

The **Debug** panel on the right lists every module scope with a heartbeat
(`invalidateLater` observer), a ping counter (observer on a root `reactiveVal`, fired
by **Ping all scopes**) and its live input count. After removing a Dataset its
rows must freeze, its pings stay put and its input count drops to 0.
