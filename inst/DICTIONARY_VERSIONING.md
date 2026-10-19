# App category dictionary version policy

Approved 2026-10-19. Dictionary versions use `vMAJOR.MINOR.PATCH` independently
of the appusageR software version and scientific output schema.

| Increment | Trigger | Example |
|---|---|---|
| PATCH | Classify existing Others / Unclassified identities; correct existing labels; clean, merge, normalize or adjust rows without introducing a new app identity or changing the taxonomy | v2.0.1 -> v2.0.2 |
| MINOR | Add one or more app identities never represented in the dictionary, keeping the taxonomy unchanged | v2.0.1 -> v2.1.0 |
| MAJOR | Change the classification framework, label vocabulary, category definitions or L1/L2 parent relationships; this includes adding Others / Unclassified | v1.0.0 -> v2.0.0 |

Use the highest applicable increment once per approved release. Increment by one;
reset PATCH to zero on MINOR, and both MINOR and PATCH on MAJOR. Thus v2.2.0 is
the next minor release after v2.1.x, not a skipped increment from v2.0.1.
Adding an alias row for an already represented identity is a PATCH; a genuinely
new app is a MINOR even if its initial label is Others / Unclassified. Changing
an app's assignment among existing category pairs is a PATCH; changing what the
categories mean or how they relate is a MAJOR. Dataset cleanup must not erase
source provenance. Never overwrite a released snapshot under the same version.

## Version ledger

| Dictionary version | Former identifier | Content |
|---|---|---|
| v1.0.0 | v261013 | Corrected manual dictionary: 11,849 rows, 14 L1 / 31 L2 functional labels |
| v2.0.0 | v261019 | 28,088 rows; adds the operational Others / Unclassified pair and the user-approved identity extension |

The legacy date identifiers remain aliases in provenance and historical filenames.
Relabeling v261019 as v2.0.0 changes version metadata only; its values, types, order,
names and missingness are identical. v261012 remains a historical pre-v1.0.0
snapshot without a newly invented semantic version.

## Packaging and provenance

The public dataset remains a five-character-column data frame:
`App_Name`, `App_Name_Repaired`, `App_UUID`, `Level_1_Category`, `Level_2_Category`.
Store version/source/license information in `dictionary_provenance`, not repeated
columns. Confidence, evidence URLs, review IDs and usage statistics remain outside
the public dictionary. GPL-3 attribution is recorded in `COPYRIGHTS`.

`DESCRIPTION` versions software; `dictionary_version` versions the dictionary;
scientific schema versions describe output structure/semantics. A dictionary-only
change does not force a software or scientific-schema bump. A later published
package artifact containing changed data still needs a distinct package release
number and the usual release approval. Record dictionary version and content hash
in category provenance; version text alone never proves artifact equivalence.
Existing caches and DataBox releases retain their recorded dictionary until an
explicitly authorized category update. Residual labels are provisional and do not
prove low usage, a functional category, or completed independent review.
