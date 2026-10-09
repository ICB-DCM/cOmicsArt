# PROTOTYPE 06 -- MultiOmics shell (throwaway)

Answers ticket 06 (`.scratch/multi-omics/tickets/06-shell-prototype.md`): how the
`Upload | Datasets | MultiOmics` shell looks and behaves, with Datasets created
dynamically and torn down with `session$destroy()`. No real analysis. Not part of
the app; delete once the decisions are folded into the plan.

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

## Run (Docker, alternative, not verified)

The app image has R 4.2.1 (amd64). shiny 1.14 needs R >= 4.1, so this should work
but was not tried:

```sh
docker run --rm -p 3939:3939 -v "$PWD/prototypes/06-shell:/proto" --entrypoint R \
  pauljonasjost/comicsart:latest -e 'lib <- tempfile(); dir.create(lib); .libPaths(c(lib, .libPaths())); install.packages("shiny", lib = lib, repos = "https://cloud.r-project.org"); shiny::runApp("/proto", host = "0.0.0.0", port = 3939)'
```

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
