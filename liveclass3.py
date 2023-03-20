# If-Else Statement
## Pembuktian If-else series
val = 100
if val >= 100:
   print("Nilai lebih besar sama dengan 100") # Output yang tercetak, karena Python berjalan secara series.
elif val == 100:
   print("Nilai sama dengan 100")

## Pengecekan nilai alphabet berdasarkan nilai angka
na = 75

if na >= 90:
   print("Grade A")
elif na >= 80 and na < 90:
   print("Grade B")
elif na >= 70 and na < 80:
   print("Grade C")
elif na >= 60 and na < 70:
   print("Grade D")
else:
   print("Grade E")

## Kasus Soto
uang = 20000

if uang >= 20000:
   print("Padang")
elif uang >= 15000 and uang < 20000:
    print("Soto")
elif uang >= 10000 and uang < 15000:
    print("Batagor")
elif uang >= 5000 and uang < 10000:
    print("Pop Ice")
else:
    print("Tidak makan dan minum")

# Looping
for i in range(1, 10, 2):
   print(i)

i = 0
while i < 10:
   print(i+1)
   i = i + 1

listku = ['apel', 'pisang', 'melon', 'semangka']
for i in listku:
   if i == 'pisang':
      print("Pisang ada di listku")
   else:
      print("Pisang tidak ada di listku")


# Define function
# Challenge 1
# Menghitung nilai akhir
def check_grade(na):
    if na >= 80:
        print("Anda lulus.")
    else:
        print("Anda tidak lulus.")

# Challenge 2
# Fungsi menghitung luas lingkaran
def areacircle(r):
   return 3.14 * r ** 2 #Artinya pi * r^2

# Challenge 3
# Fungsi untuk menghitung a dan b berdasarkan status yang diberikan
def hitung(a, b, status):
    if status == 'tambah':
       return a + b
    elif status == 'kali':
       return a * b
    elif status == 'bagi':
       return a / b