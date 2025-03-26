#!/bin/bash


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

echo "=== Ada Hesap Makinesi Testleri ==="

if ! command -v gnatmake &> /dev/null; then
    echo "GNAT (Ada derleyicisi) kurulu değil."
    echo "Testler çalıştırılamadı."
    exit 1
fi

echo "Ada hesap makinesi derleniyor..."
gnatmake calculator.adb || { echo "Ada derlemesi başarısız oldu!"; exit 1; }

for i in "${!TEST_CASES[@]}"; do
    test_case="${TEST_CASES[$i]}"
    
    echo "Test $((i+1)): '${test_case}'"
    result=$(echo -e "${test_case}\nexit" | ./calculator | grep -v "Ada Hesap Makinesi" | grep -v "Aritmetik ifadeleri girin" | grep -v "Çıkmak için" | grep -v "Hoşçakalın!" | grep -v "^> $")
    echo "$result"
    echo ""
done

# Temizlik
echo -e "\nDerlenen dosyalar temizleniyor..."
rm -f calculator *.o *.ali 