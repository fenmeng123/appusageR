#' Two-level app category dictionary with provisional residual labels
#'
#' Dictionary v2.0.0 (formerly v261019) combines the corrected v1.0.0
#' (formerly v261013) manual dictionary
#' with user-approved high- and medium-confidence app labels and provisional
#' residual assignments. Dictionary versions are independent of package versions.
#'
#' @format A base data frame with 28,088 rows and five character columns:
#' \describe{
#'   \item{App_Name}{Original app display name.}
#'   \item{App_Name_Repaired}{Manually repaired app display name; may be missing.}
#'   \item{App_UUID}{Source app identity, matched against Android package names.
#'     This legacy column name does not mean a participant UUID.}
#'   \item{Level_1_Category}{One of 14 functional categories, or \code{Others}.}
#'   \item{Level_2_Category}{One of 31 functional categories, or \code{Unclassified}.}
#' }
#' @details
#' The taxonomy is informed by Uses and Gratifications and classifies app
#' functions. Its broad theoretical orientations are nonexclusive interpretations,
#' not a third coded level or an observation of an individual user's motives.
#'
#' The first 11,849 rows preserve v1.0.0 values, order, missing repaired names
#' and duplicate identities. The extension contains 16,239 saved name/alias rows
#' for 14,118 additional app identities. The user approved 726 high-confidence
#' and 1,199 medium-confidence functional labels from the independently reviewed
#' 6,052-identity snapshot. The other 12,193 identities receive
#' \code{Others / Unclassified}, including low-confidence, untrusted and
#' unreviewed identities. These residuals do not establish low usage or an app
#' function and are not a fifteenth validated functional category. They can be
#' replaced in a later dictionary snapshot.
#'
#' No conflicting normalized package mappings are introduced. App names can
#' still be ambiguous; the matching functions retain their existing conflict
#' abstention. Evidence, confidence ratings, review IDs and usage statistics are
#' deliberately excluded from the five-column public dataset. The extension is
#' not new reliability or validity evidence for the original working manuscript.
#'
#' The \code{dictionary_provenance} attribute records the version, parent mapping
#' version, source basename, SHA-256, approval scope and conversion contract.
#' Original workbooks and participant-level validation data are not included.
#'
#' This bundled snapshot is redistributed under GPL-3, the package's license.
#' See the installed \file{COPYRIGHTS} file for dictionary attribution.
#' @source Corrected \code{apptyp_dictionary_v261013.xlsx}, \code{Sheet1},
#' approved 2026-10-12, plus the user's approved independent-review snapshot
#' extension of 2026-10-19. Historical date-form identifiers remain provenance
#' aliases. The installed \file{DICTIONARY_VERSIONING.md} defines the independent
#' major/minor/patch policy: existing-identity fixes, new identities, and taxonomy
#' changes have separate increment levels. Version metadata does not add columns.
#' @references Wang, Z., Jin, X., Zhang, C., Zhang, J., Zheng, H., Hu, Q.,
#' and Song, K. (2026). \emph{Development and initial validation of a smartphone
#' app taxonomy informed by uses and gratifications theory}. Working manuscript.
#' @seealso [add_app_categories()], [read_app_category_dictionary()]
#' @examples
#' data("app_category_dictionary", package = "appusageR")
#' attr(app_category_dictionary, "dictionary_provenance")$dictionary_version
#' head(unique(app_category_dictionary$Level_1_Category))
#' @keywords datasets
"app_category_dictionary"
