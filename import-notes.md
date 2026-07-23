# RWP-Oncology import

## Structure

```
Epic     tumor product family        Prostate Panoramic
└── Feature   type + version          RWDP 2026m03
    └── PBI   stage, tagged           Programming
```

7 Epics, 16 Features, 33 stage PBIs.

## Stage sets

**RWDP** — Methodology Doc → Prog Spec → Prog Spec QC → Programming → Program Validation → Release Prep
**Dashboard** — Requirements → Design → Build → UAT → Release Prep
**RWDP, no logic change** — Program Execution → Program Validation → Release Prep

Each PBI carries its stage name as a plain tag. Query `Tags Contains <stage name>`.

## Features with no stage PBIs

Six released products and three unresolved ones. The Feature stands as the record;
creating stage PBIs for work already finished would put cutover dates on history that
never happened.

| Feature | Why |
|---|---|
| Prostate Panoramic → Dashboard | Shipped? Version unknown |
| advNSCLC → Dashboard | Stage and date unknown |
| mCRC → RWDP 2026m03 | Released |
| mCRC → RWDP 2026m06 | Refresh — delivery path unresolved |
| Ovarian Cancer → RWDP 2026m03, Dashboard 2026m05 | Released |
| Panoramic SCLC → all three | Released |
| DLBCL COTA → RWDP | Shipped? |

Three Features have no version suffix — the dates were unknown in the old tracker.
Rename them in ADO once you know: `Dashboard` → `Dashboard 2026m07`.

## Import

1. **Delete the two test items first** — Feature 2362329 and PBI 2362330. Otherwise
   Prostate Panoramic RWDP duplicates.
2. Boards → Queries → **Import work items** → `RWP_import.csv`
3. Everything arrives at State `New`. Set states after:
   - Released Features → Closed
   - Active Features → In Progress
   - Stage PBIs: finished stages → Done, current stage → In Progress, later → New

## After import

**Queries** to build under Shared Queries:

| Query | Filter |
|---|---|
| Active pipeline | Work Item Type = Feature, State <> Done |
| Completed | Work Item Type = Feature, State = Done |
| Current work | Work Item Type = PBI, State = In Progress |
| Unassigned | Work Item Type = PBI, Assigned To = blank, State <> Done |
| By stage | Work Item Type = PBI, Tags Contains \<stage\> |

Add Tags as a displayed column so the stage shows in results.

## Not included

**RWD Dashboard Enhancement** — cross-cutting improvement work across dashboards, not a
product. Doesn't fit the Epic-per-family model. Add it by hand wherever it belongs once
you decide.
