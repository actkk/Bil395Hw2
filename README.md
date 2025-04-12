

Bu proje, 5 farklı programlama dilinde (Rust, Prolog, Ada, Scheme ve Perl) hesap makinesi uygulaması geliştirmeyi ve test etmeyi amaçlamaktadır.

## Proje Yapısı

Her programlama dili için ayrı bir klasör bulunmaktadır:
- `rust/` - Rust ile yazılmış hesap makinesi
- `prolog/` - Prolog ile yazılmış hesap makinesi
- `ada/` - Ada ile yazılmış hesap makinesi
- `scheme/` - Scheme ile yazılmış hesap makinesi
- `perl/` - Perl ile yazılmış hesap makinesi

Her klasör içinde:
- Hesap makinesi uygulaması (`calculator.{rs|pl|adb|scm|pl}`)
- Test scripti (`test_<dil>.sh`)

## Hesap Makinesi Özellikleri

Her dildeki hesap makinesi aşağıdaki özelliklere sahiptir:
- Temel aritmetik işlemler (toplama, çıkarma, çarpma, bölme)
- İşlem önceliği desteği 
- Hata yönetimi (sıfıra bölme, geçersiz karakter vb.)

## Test Sistemi

Her dil için ayrı bir test scripti bulunmaktadır. Seneryolar aşağıdaki gibidir :

1. Basit toplama: `5+5` = 10
2. Basit çıkarma: `10-4` = 6
3. Basit çarpma: `3*7` = 21
4. Basit bölme: `8/2` = 4
5. İşlem önceliği: `5+5*2` = 15
6. İşlem önceliği: `10/2+3` = 8
7. Karmaşık ifade: `2*3+4*5` = 26
8. Çoklu işlem: `5+5+5+5` = 20
9. Hata durumu: `8/0` = Sıfıra bölme hatası
10. Hata durumu: `5+a` = Geçersiz karakter hatası

### Testleri Çalıştırma

Tüm testleri çalıştırmak için ana dizinde:
```
./run_all_tests.sh
```

Belirli bir dil için test çalıştırmak için:
```
cd <dil_adı>
./test_<dil_adı>.sh
```

## Docker ile Çalıştırma

MacOS üzerinde Ada dilini compile ederken problemler yaşadım. GNAT kurulumunu gerçekleşitremedim buna çözüm olarak Docker kullandım ve bunun için Dockerfile ekledim

### Docker İmajını Oluşturmak ve Çalıştırmak

Docker imajını oluşturmak için:
```
docker build -t multi-lang-calculator .
```

Oluşturulan imajı çalıştırmak için:
```
docker run multi-lang-calculator
```

Bu komut, tüm programlama dilleri için hesap makinesi testlerini otomatik olarak çalıştırır ve sonuçlarını ekrana yazdırır.

### Docker İmajının İçeriği

Docker imajı içeriği:
- Ubuntu 22.04
- GNAT (Ada derleyicisi)
- Rust derleyicisi
- SWI-Prolog yorumlayıcısı
- Guile Scheme yorumlayıcısı
- Perl yorumlayıcısı ve Error modülü
- Türkçe desteği

## Kod Çalışma Mantığı

### Rust Hesap Makinesi
- Tokenization: Giriş stringi analiz edilip tokenlara ayrılır (sayılar, operatörler)
- Recursive descent parser ile işlem önceliği sağlanır
- Ayrıştırma işlemi `parse_expression`, `parse_term` ve `parse_factor` fonksiyonları ile gerçekleştirilir
- İşlem önceliği bu fonksiyonlar sayesinde doğru sırayla uygulanır (önce çarpma/bölme, sonra toplama/çıkarma)
- Hata durumları özel hata türleri ile yönetilir

### Prolog Hesap Makinesi
- DCG (Definite Clause Grammar) ile giriş stringi ayrıştırılır
- Recursive descent parser kullanılarak işlem önceliği sağlanır
- İşlemler değerlendirilir ve sonuç hesaplanır
- Predicate'ler ile hata durumları yönetilir

### Scheme Hesap Makinesi
- Recursive descent parser kullanılır
- Lisp tarzı değerlendirme ile AST işlenir
- İşlem önceliği ve hesaplama yapılır
- Exception handling ile hatalar yönetilir

### Perl Hesap Makinesi
- Regex ile tokenization yapılır
- Shunting yard algoritması ile infix'ten postfix'e çevrilir
- Postfix notasyonu değerlendirilerek sonuç hesaplanır
- Die/warn mekanizmaları ile hata yönetimi sağlanır

### Ada Hesap Makinesi
- Recursive descent parser ile ifadeler ayrıştırılır
- İşlem önceliği için ayrı ayrı fonksiyonlar kullanılır
- Strong typing ile tip güvenliği sağlanır
- Exception handling ile hata durumları yönetilir

### Test Scriptleri
Her test scripti:
1. Hesap makinesi uygulamasını başlatır
2. Test vakalarını sırayla çalıştırır
3. Sonuçları ekrana yazdırır
4. Programı düzgün şekilde sonlandırır

Test scriptlerinin çalışması:
- Her test girdisi piping ile hesap makinesi uygulamasına aktarılır
- Her test vakasından sonra "exit" komutu gönderilerek uygulama düzgün şekilde kapatılır
- Sonuçlar terminal ekranına yazdırılır

### Ana Test Scripti (run_all_tests.sh)
Bu script:
1. Tüm dillerin listesini tutar
2. Her dil için ilgili test scriptini çalıştırır
3. Test scriptlerinin çıktılarını birleştirerek tek bir rapor oluşturur

