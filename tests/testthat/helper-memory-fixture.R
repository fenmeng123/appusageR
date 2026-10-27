# Resource-policy tests supply explicit low/unknown-memory inputs. Other CRAN
# tests use tiny synthetic files, so a deterministic host avoids spawning a
# PowerShell/CIM process for every cache operation. Exercise the real detector
# separately; local acceptance, including eight workers, keeps real detection.
appusage_test_active_memory_probe <- getFromNamespace("appusage_detect_memory_info", "appusageR")
appusage_test_uses_memory_fixture <- !is.null(attr(appusage_test_active_memory_probe, "real_probe"))
appusage_test_real_memory_probe <- attr(appusage_test_active_memory_probe, "real_probe")
if (is.null(appusage_test_real_memory_probe))
  appusage_test_real_memory_probe <- appusage_test_active_memory_probe
appusage_test_memory_fixture <- list(
  detected_total_memory_bytes = 64 * 1024^3,
  detected_free_memory_bytes = 32 * 1024^3,
  memory_detected = TRUE,
  memory_source = "synthetic_cran_test_fixture"
)
