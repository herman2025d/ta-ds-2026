# ==============================================================================
# TEKS APLIKASI ML: label pilihan (Bahasa Indonesia) + rekomendasi per profil
# Khusus aplikasi (tidak disalin dari script/). Label kode diterjemahkan dari
# label nilai SPSS file PISA 2025 (data_raw/PISA2025_STU_IDN.rds).
# ==============================================================================

# 1. Pertanyaan kalkulator ringkas (butir asli PISA 2025, LOG ML-24/25) ---------
# Kunci = nama prediktor model ringkas (skrip 07). `pilihan` = kode asli PISA ->
# terjemahan label nilai SPSS; urutan elemen = urutan TAMPIL (selalu dari
# negatif/rendah ke positif/tinggi agar konsisten, walau kode PISA terbalik).
.setuju4 <- c(`1` = "Sangat tidak setuju", `2` = "Tidak setuju", `3` = "Setuju", `4` = "Sangat setuju")
.isced <- c(`1` = "Belum tamat SMP", `2` = "SMP sederajat",
            `3` = "SMA/SMK tanpa akses langsung ke perguruan tinggi", `4` = "SMA/SMK sederajat",
            `5` = "Pasca-menengah nontersier", `6` = "Diploma (D1–D3)", `7` = "S1/D4",
            `8` = "S2", `9` = "S3")

PERTANYAAN <- list(
  ST094Q05NA = list(
    konstruk = "JOYSCIE", kelompok = "sikap",
    teks = "Saya tertarik mempelajari sains.",
    pilihan = .setuju4),
  ST129Q08DA = list(
    konstruk = "EFFSCIE", kelompok = "sikap",
    teks = paste("Seberapa mudah bagimu melakukan tugas ini sendiri: menentukan mana yang lebih",
                 "baik dari dua penjelasan tentang penyebab perubahan iklim?"),
    pilihan = c(`4` = "Saya tidak bisa melakukannya", `3` = "Saya akan kesulitan melakukannya sendiri",
                `2` = "Saya bisa dengan sedikit usaha", `1` = "Saya bisa dengan mudah")),
  ST301Q04JA = list(
    konstruk = "CURIO", kelompok = "sikap",
    teks = "Saya suka mengetahui bagaimana sesuatu bekerja.",
    pilihan = c(`1` = "Sangat tidak setuju", `2` = "Tidak setuju", `3` = "Netral",
                `4` = "Setuju", `5` = "Sangat setuju")),
  ST433Q04DA = list(
    konstruk = "SELFREG", kelompok = "sikap",
    teks = "Ketika saya kesulitan belajar, saya meminta bantuan.",
    pilihan = c(`4` = "Sangat tidak setuju", `3` = "Tidak setuju", `2` = "Setuju", `1` = "Sangat setuju")),
  ST092Q08NA = list(
    konstruk = "ENVAWARE", kelompok = "sains",
    teks = "Seberapa paham kamu tentang isu lingkungan ini: kepunahan tumbuhan dan hewan?",
    pilihan = c(`1` = "Belum pernah mendengarnya",
                `2` = "Pernah mendengar, tetapi tidak bisa menjelaskan maksudnya",
                `3` = "Tahu sedikit dan bisa menjelaskan garis besarnya",
                `4` = "Paham dan bisa menjelaskannya dengan baik")),
  ST515Q09DA = list(
    konstruk = "ENGSCIPR", kelompok = "sains",
    teks = paste("Seberapa sering, di pelajaran sainsmu, kamu menyusun argumen ilmiah dengan",
                 "bukti untuk mendukung suatu klaim?"),
    pilihan = c(`4` = "Tidak pernah atau hampir tidak pernah", `3` = "Di sebagian pelajaran",
                `2` = "Di sebagian besar pelajaran", `1` = "Di setiap pelajaran")),
  REPEAT = list(
    konstruk = "REPEAT", kelompok = "diri",
    teks = "Apakah kamu pernah tinggal kelas?",
    pilihan = c(`Student never repeated a grade` = "Tidak pernah",
                `Student repeated a grade at least once` = "Pernah, minimal sekali")),
  WORKPAY = list(
    konstruk = "WORKPAY", kelompok = "diri",
    teks = "Dalam seminggu, berapa kali kamu bekerja dengan dibayar sebelum atau sesudah sekolah?",
    pilihan = c(`0` = "Tidak bekerja", setNames(paste(1:9, "kali"), 1:9), `10` = "10 kali atau lebih")),
  EXPECEDU = list(
    konstruk = "EXPECEDU", kelompok = "diri",
    teks = "Jenjang pendidikan tertinggi apa yang ingin kamu selesaikan?",
    pilihan = .isced),
  HISCED = list(
    konstruk = "HISCED", kelompok = "diri",
    teks = "Apa pendidikan tertinggi ayah atau ibumu (pilih yang paling tinggi)?",
    pilihan = c(`1` = "Tidak memiliki ijazah SMP", .isced[-1]))
)

KELOMPOK_TANYA <- c(sikap = "Sikap belajarmu", sains = "Kamu dan sains", diri = "Tentang dirimu")

# 3. Rekomendasi pembelajaran per profil ----------------------------------------
# Kunci = nama profil otomatis (nama_profil()). Disusun dari literatur pedagogi;
# BUKAN keluaran model dan belum diuji efektivitasnya pada data ini.
REKOMENDASI <- list(
  "Kemampuan rendah, sikap belajar rendah" = list(
    fokus = "Bangun keyakinan diri dan minat sambil memperkuat konsep prasyarat.",
    langkah = c(
      "Tugas berjenjang dengan keberhasilan kecil yang terlihat (pengalaman keberhasilan; Bandura, 1997).",
      "Konteks sains yang dekat dengan kehidupan siswa untuk memicu minat situasional (Hidi & Renninger, 2006).",
      "Pengajaran eksplisit dan terbimbing untuk konsep dasar, bukan penemuan bebas (Kirschner dkk., 2006).",
      "Umpan balik formatif singkat dan sering (Black & Wiliam, 1998).")),
  "Kemampuan rendah, sikap belajar sedang" = list(
    fokus = "Tutup celah konsep; motivasi relatif sudah ada dan perlu dijaga.",
    langkah = c(
      "Asesmen diagnostik awal untuk menemukan miskonsepsi.",
      "Contoh terurai (worked example) yang bantuannya dikurangi bertahap (Kirschner dkk., 2006).",
      "Umpan balik spesifik tentang tugas dan proses agar keyakinan diri sejalan dengan capaian (Hattie & Timperley, 2007).",
      "Pantau kemajuan secara berkala dan rayakan peningkatan.")),
  "Kemampuan rendah, sikap belajar tinggi" = list(
    fokus = "Salurkan motivasi yang tinggi ke strategi belajar yang efektif.",
    langkah = c(
      "Ajarkan strategi belajar, perencanaan, dan pemantauan diri (Zimmerman, 2002).",
      "Inkuiri terbimbing dengan perancah (scaffolding) yang jelas (Hmelo-Silver dkk., 2007).",
      "Remediasi konsep inti secara terarah.",
      "Umpan balik kemajuan agar motivasi tidak turun saat hasil belum terlihat.")),
  "Kemampuan tinggi, sikap belajar tinggi" = list(
    fokus = "Pengayaan dan pengembangan penalaran ilmiah tingkat tinggi.",
    langkah = c(
      "Proyek investigasi atau inkuiri yang lebih terbuka.",
      "Tugas mengevaluasi desain penyelidikan dan kualitas bukti ilmiah.",
      "Peran tutor sebaya dalam kerja kelompok.",
      "Kaitkan dengan karier dan isu sains di masyarakat.")),
  "Kemampuan tinggi, sikap belajar sedang" = list(
    fokus = "Tumbuhkan keterlibatan dan regulasi diri agar kemampuan terus berkembang.",
    langkah = c(
      "Tugas autentik yang memberi pilihan (otonomi; Ryan & Deci, 2000).",
      "Penetapan tujuan dan refleksi belajar (Zimmerman, 2002).",
      "Tantangan bertingkat agar siswa tidak bosan.",
      "Tunjukkan relevansi sains dengan minat dan rencana siswa.")),
  "Kemampuan tinggi, sikap belajar rendah" = list(
    fokus = "Cegah menurunnya keterlibatan pada siswa yang sebenarnya mampu.",
    langkah = c(
      "Beri relevansi dan otonomi dalam tugas (Ryan & Deci, 2000).",
      "Hubungkan materi dengan minat pribadi siswa (Hidi & Renninger, 2006).",
      "Peran bermakna dalam kerja kelompok.",
      "Dialog guru–siswa tentang tujuan belajar."))
)

# 4. Saran dari jawaban siswa (per aspek yang dijawab rendah) --------------------
# `pemicu` = kode jawaban PISA yang memunculkan saran (separuh bawah skala).
# Aspek yang bisa dipengaruhi pembelajaran -> saran berbasis teori.
# Aspek latar (tinggal kelas, bekerja) -> hanya "perhatian konteks", bukan saran
# pedagogis, karena keduanya riwayat/kondisi, bukan hal yang diubah di kelas.
SARAN_BUTIR <- list(
  ST094Q05NA = list(pemicu = c("1", "2"), jenis = "saran",
    judul = "Minat belajar sains masih rendah",
    teks = paste("Mulai dari minat situasional: fenomena yang mengejutkan, demonstrasi, dan masalah nyata",
                 "di sekitar siswa. Beri ruang bertanya dan bereksplorasi agar minat berkembang menjadi",
                 "minat pribadi yang bertahan."),
    teori = "Model empat fase perkembangan minat (Hidi & Renninger, 2006)"),
  ST129Q08DA = list(pemicu = c("3", "4"), jenis = "saran",
    judul = "Keyakinan diri dalam sains masih rendah",
    teks = paste("Rancang tugas bertahap yang memberi pengalaman berhasil nyata, tunjukkan contoh teman",
                 "sebaya yang berhasil, dan beri umpan balik yang menekankan usaha serta strategi, bukan bakat."),
    teori = "Teori efikasi diri: pengalaman keberhasilan dan pemodelan (Bandura, 1997)"),
  ST301Q04JA = list(pemicu = c("1", "2"), jenis = "saran",
    judul = "Rasa ingin tahu belum tampak",
    teks = paste("Buka pelajaran dengan pertanyaan terbuka, teka-teki, atau meminta siswa menebak hasil",
                 "percobaan sebelum dilakukan. Celah antara tebakan dan hasil memancing rasa ingin tahu."),
    teori = "Teori celah informasi tentang rasa ingin tahu (Loewenstein, 1994)"),
  ST433Q04DA = list(pemicu = c("3", "4"), jenis = "saran",
    judul = "Jarang meminta bantuan saat kesulitan",
    teks = paste("Jadikan bertanya hal yang wajar di kelas, sediakan jalur bantuan yang aman (tutor sebaya,",
                 "waktu konsultasi), dan ajarkan kapan serta bagaimana meminta bantuan sebagai strategi belajar."),
    teori = "Pencarian bantuan adaptif dalam regulasi diri (Newman, 2002; Zimmerman, 2002)"),
  ST092Q08NA = list(pemicu = c("1", "2"), jenis = "saran",
    judul = "Pemahaman isu lingkungan masih terbatas",
    teks = paste("Kaitkan materi sains dengan isu lingkungan setempat (sampah, banjir, keanekaragaman hayati)",
                 "melalui pembelajaran berbasis masalah atau proyek, disertai bimbingan dan diskusi hasil."),
    teori = "Pembelajaran berbasis masalah (Hmelo-Silver, 2004)"),
  REPEAT = list(pemicu = "Student repeated a grade at least once", jenis = "konteks",
    judul = "Pernah tinggal kelas",
    teks = paste("Di data PISA, riwayat tinggal kelas berkaitan kuat dengan capaian di bawah Level 2.",
                 "Pantau kemajuan lebih awal dan pastikan konsep prasyarat sudah dikuasai.")),
  WORKPAY = list(pemicu = as.character(3:10), jenis = "konteks",
    judul = "Bekerja dibayar 3 kali atau lebih per minggu",
    teks = paste("Waktu dan tenaga untuk belajar mungkin terbatas. Koordinasikan dengan wali kelas, guru BK,",
                 "dan keluarga; pertimbangkan tugas dengan waktu pengerjaan yang lebih fleksibel."))
)

KERANGKA_REKOMENDASI <- paste(
  "Rekomendasi disusun dalam kerangka pembelajaran berdiferensiasi, yaitu menyesuaikan pembelajaran",
  "dengan kesiapan, minat, dan profil belajar siswa (Tomlinson, 2017). Pendekatan ini sejalan dengan",
  "pembelajaran berdiferensiasi dalam Kurikulum Merdeka.")

CATATAN_REKOMENDASI <- paste(
  "Saran disusun dari literatur pedagogi umum, bukan keluaran model, dan belum diuji",
  "efektivitasnya pada data ini. Gunakan sebagai bahan pertimbangan guru, bukan untuk",
  "keputusan berisiko tinggi (penempatan, kenaikan kelas, seleksi).")

PUSTAKA <- c(
  "Bandura, A. (1997). Self-efficacy: The exercise of control. W. H. Freeman.",
  "Black, P., & Wiliam, D. (1998). Assessment and classroom learning. Assessment in Education, 5(1), 7–74.",
  "Hathaway, R. J., & Bezdek, J. C. (2001). Fuzzy c-means clustering of incomplete data. IEEE Transactions on Systems, Man, and Cybernetics, Part B, 31(5), 735–744.",
  "Hattie, J., & Timperley, H. (2007). The power of feedback. Review of Educational Research, 77(1), 81–112.",
  "Hidi, S., & Renninger, K. A. (2006). The four-phase model of interest development. Educational Psychologist, 41(2), 111–127.",
  "Hmelo-Silver, C. E. (2004). Problem-based learning: What and how do students learn? Educational Psychology Review, 16(3), 235–266.",
  "Hmelo-Silver, C. E., Duncan, R. G., & Chinn, C. A. (2007). Scaffolding and achievement in problem-based and inquiry learning. Educational Psychologist, 42(2), 99–107.",
  "Kirschner, P. A., Sweller, J., & Clark, R. E. (2006). Why minimal guidance during instruction does not work. Educational Psychologist, 41(2), 75–86.",
  "Loewenstein, G. (1994). The psychology of curiosity: A review and reinterpretation. Psychological Bulletin, 116(1), 75–98.",
  "Newman, R. S. (2002). How self-regulated learners cope with academic difficulty: The role of adaptive help seeking. Theory Into Practice, 41(2), 132–138.",
  "Ryan, R. M., & Deci, E. L. (2000). Self-determination theory and the facilitation of intrinsic motivation, social development, and well-being. American Psychologist, 55(1), 68–78.",
  "Tomlinson, C. A. (2017). How to differentiate instruction in academically diverse classrooms (3rd ed.). ASCD.",
  "Zimmerman, B. J. (2002). Becoming a self-regulated learner: An overview. Theory Into Practice, 41(2), 64–70."
)
