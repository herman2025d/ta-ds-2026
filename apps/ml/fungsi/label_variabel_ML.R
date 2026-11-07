# ==============================================================================
# LABEL VARIABEL ML (Bahasa Indonesia) — dipakai grafik (05) dan prototipe Shiny
# Terjemahan dari label asli OECD di data_clean/ML_kamus_variabel.csv
# ==============================================================================

LABEL_ML <- c(
  ESCS = "Status sosial-ekonomi-budaya", HOMEPOS = "Kepemilikan barang di rumah",
  HISEI = "Status pekerjaan orang tua", AGE = "Usia",
  BELONG = "Rasa memiliki sekolah", BULLIED = "Pengalaman dirundung",
  FEELSAFE = "Rasa aman di sekolah", FAMSUP = "Dukungan keluarga",
  DISCLISCI = "Iklim disiplin kelas sains", TEACHSUP = "Dukungan guru sains",
  COGACSC = "Aktivasi kognitif kelas sains", SCIECOLLR = "Penalaran kolaboratif sains",
  JOYSCIE = "Kesenangan belajar sains", EFFSCIE = "Keyakinan diri dalam sains",
  ENGSCIPR = "Keterlibatan dalam praktik sains", CURIO = "Rasa ingin tahu",
  ENPROBS = "Kesenangan memecahkan masalah", COGABIL = "Adaptabilitas kognitif",
  GOALSET = "Penetapan tujuan", SELFREG = "Mengatur diri saat belajar",
  AIUSESCH = "Penggunaan AI di sekolah", OPENVLRN = "Kesempatan belajar isu lingkungan",
  ENVAPART = "Partisipasi kegiatan lingkungan", COLLENEFF = "Efikasi kolektif lingkungan",
  ENVCAPCH = "Keyakinan mampu mengubah lingkungan", ENVAWARE = "Pemahaman isu lingkungan",
  WORKPAY = "Bekerja dibayar di luar sekolah", EXERPRAC = "Berolahraga di luar sekolah",
  HISCED = "Pendidikan tertinggi orang tua", EXPECEDU = "Harapan jenjang pendidikan",
  TARDYSD = "Datang terlambat", GRADE = "Kelas relatif thd kelas modal",
  MALE = "Jenis kelamin", IMMIG = "Status imigran", REPEAT = "Pernah tinggal kelas",
  SKIPPING = "Membolos", SISCO = "Kejelasan cita-cita pekerjaan",
  CLSIZE = "Ukuran kelas", EDULEAD = "Kepemimpinan pembelajaran kepsek",
  EDUSHORT = "Kekurangan sarana belajar", STAFFSHORT = "Kekurangan tenaga pendidik",
  NEGSCLIM = "Iklim sekolah negatif", DIGPREP = "Kesiapan belajar digital",
  ALLACTIV = "Kegiatan ekstrakurikuler", AVLRSOFT = "Ketersediaan perangkat lunak belajar",
  PROATCE = "Proporsi guru bersertifikat", SCHSIZE_Q = "Ukuran sekolah",
  STRATIO_Q = "Rasio siswa-guru", CREACTIV = "Ekstrakurikuler kreatif",
  SCHLTYPE = "Jenis sekolah", SCHSEL = "Selektivitas sekolah",
  ABGSCIE = "Pengelompokan kemampuan kelas sains",
  # fitur klaster
  SAINS = "Kemampuan sains", SEPS = "Menjelaskan fenomena secara ilmiah",
  SEDE = "Mengevaluasi desain penyelidikan ilmiah",
  SEID = "Mengevaluasi informasi ilmiah untuk keputusan"
)

# Penjelasan istilah untuk pembaca awam (tooltip Shiny, catatan grafik).
# Istilah teknis tetap dipakai di teks laporan; label di atas = bahasa sehari-hari.
ISTILAH_ML <- c(
  EFFSCIE = "efikasi diri: seberapa yakin siswa mampu mengerjakan tugas sains sendiri",
  JOYSCIE = "seberapa senang siswa saat mempelajari topik sains",
  CURIO   = "keinginan siswa mengetahui dan mencoba hal-hal baru",
  SELFREG = paste("regulasi diri: mengelola cara belajar sendiri; di PISA 2025 indeks ini paling kuat",
                  "diwakili kebiasaan meminta bantuan saat kesulitan belajar"),
  ENVAWARE = "seberapa paham siswa tentang isu lingkungan (mis. gas rumah kaca, sampah plastik)",
  ENGSCIPR = "seberapa sering siswa melakukan praktik sains di kelas (menganalisis data, menyusun argumen dan penjelasan ilmiah)",
  REPEAT   = "riwayat pernah mengulang (tinggal) kelas",
  WORKPAY  = "bekerja dengan dibayar sebelum atau sesudah sekolah",
  EXPECEDU = "jenjang pendidikan tertinggi yang ingin dicapai siswa",
  HISCED   = "pendidikan tertinggi salah satu orang tua",
  SIKAP   = "sikap belajar (disposisi belajar): keyakinan diri, kesenangan, rasa ingin tahu, dan mengatur diri"
)

# Nama profil belajar OTOMATIS dari pusat klaster (z-score), agar tidak dipilih
# manual: kemampuan = tanda SAINS; sikap belajar (disposisi) = rata-rata z keempat
# indikator sikap (> 0,3 tinggi, < -0,3 rendah, selain itu sedang). Dipakai grafik & Shiny.
# Nama diganti dari "disposisi" ke "sikap belajar" (7 Nov 2026, LOG ML-24) agar mudah dipahami.
AMBANG_DISPOSISI <- 0.3
nama_profil <- function(cen) {
  disp <- rowMeans(cen[, setdiff(colnames(cen), c("klaster", "SAINS")), drop = FALSE])
  kat  <- ifelse(disp > AMBANG_DISPOSISI, "tinggi", ifelse(disp < -AMBANG_DISPOSISI, "rendah", "sedang"))
  paste0("Kemampuan ", ifelse(cen[, "SAINS"] > 0, "tinggi", "rendah"), ", sikap belajar ", kat)
}

# "Label (KODE)" agar pembaca awam & teknis sama-sama terbantu
label_ml <- function(v, kode = TRUE) {
  l <- ifelse(v %in% names(LABEL_ML), LABEL_ML[v], v)
  if (kode) paste0(l, " (", v, ")") else unname(l)
}
