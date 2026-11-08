# ==============================================================================
# Label & satuan variabel EDM (dipakai skrip analisis, grafik, Shiny, laporan)
# ==============================================================================
PRED_EDM  <- c("RLS", "PENGELUARAN", "TPT", "MISKIN", "UHH", "KOTA", "DK_PERSEN")
NUM_EDM   <- setdiff(PRED_EDM, "KOTA")
PRED_UTAMA     <- PRED_EDM                      # model utama (LOG EDM-11)
PRED_PEMBANDING <- setdiff(PRED_EDM, "DK_PERSEN")

LABEL_EDM <- c(
  HLS         = "Harapan lama sekolah 2026 (tahun)",
  RLS         = "Rata-rata lama sekolah 2026 (tahun)",
  PENGELUARAN = "Pengeluaran per kapita disesuaikan 2026 (ribu Rp/orang/tahun)",
  TPT         = "Tingkat pengangguran terbuka 2025 (%)",
  MISKIN      = "Penduduk miskin 2025 (%)",
  UHH         = "Umur harapan hidup 2025 (tahun)",
  KOTA        = "Jenis wilayah (kota/kabupaten)",
  DK_PERSEN   = "Desa/kelurahan daerah khusus (%)",
  DK_JUMLAH   = "Jumlah desa/kelurahan daerah khusus"
)
# Label pendek untuk sumbu/facet
LABEL_PENDEK_EDM <- c(
  HLS = "HLS (tahun)", RLS = "RLS (tahun)", PENGELUARAN = "Pengeluaran (ribu Rp)",
  TPT = "TPT (%)", MISKIN = "Penduduk miskin (%)", UHH = "UHH (tahun)",
  KOTA = "Kota", DK_PERSEN = "Desa daerah khusus (%)", DK_JUMLAH = "Jumlah desa daerah khusus"
)
# Sisi variabel menurut kerangka ekologi (Bronfenbrenner): kondisi rumah tangga
# & penduduk (permintaan) vs hambatan wilayah (akses/pasokan)
SISI_EDM <- c(RLS = "Kondisi penduduk", PENGELUARAN = "Kondisi penduduk", TPT = "Kondisi penduduk",
              MISKIN = "Kondisi penduduk", UHH = "Kondisi penduduk", KOTA = "Wilayah",
              DK_PERSEN = "Akses wilayah")

# Kategori daerah khusus (untuk tabulasi silang; batas ditetapkan sebelum melihat target)
kategori_dk <- function(p) {
  factor(ifelse(p == 0, "Tidak ada",
         ifelse(p <= 10, "> 0–10%", ifelse(p <= 50, "> 10–50%", "> 50%"))),
         levels = c("Tidak ada", "> 0–10%", "> 10–50%", "> 50%"))
}

SUMBER_EDM <- paste0("BPS (HLS, RLS, pengeluaran 2026; TPT, P0, UHH 2025); Kepmendikbudristek ",
                     "160/P/2021; Kepmendagri 146.1-4717/2020; analisis penulis.")
