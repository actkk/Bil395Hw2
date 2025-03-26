#!/bin/bash

# Renkli çıktı tanımları kaldırıldı
# GREEN='\033[0;32m'
# RED='\033[0;31m'
# NC='\033[0m' # No Color

# Test vakalarını tanımlama
declare -a TEST_CASES=(
    "5+5"          # 10
    "10-4"         # 6
    "3*7"          # 21
    "8/2"          # 4
    "5+5*2"        # 15
    "10/2+3"       # 8
    "2*3+4*5"      # 26
    "5+5+5+5"      # 20
    "8/0"          # Hata (sıfıra bölme)
    "5+a"          # Hata (geçersiz karakter)
)

echo "=== Prolog Hesap Makinesi Testleri ==="

# Prolog (SWI-Prolog) yorumlayıcısının kurulu olup olmadığını kontrol et
if ! command -v swipl &> /dev/null; then
    echo "Prolog (SWI-Prolog) yorumlayıcısı kurulu değil."
    echo "Testler çalıştırılamadı."
    exit 1
fi

# Her bir test vakası için
for i in "${!TEST_CASES[@]}"; do
    test_case="${TEST_CASES[$i]}"
    
    echo "Test $((i+1)): '${test_case}'"
    echo -e "${test_case}\nexit" | swipl -q -f calculator.pl
    echo ""
done 