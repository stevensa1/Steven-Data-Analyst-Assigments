#If Else Statement
na = 70

def check_grade(na):
    if na >= 80:
        print("Anda lulus.")
    else:
        print("Anda tidak lulus.")

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

# Soto
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

uang = 20000
n = 10

# For loop
for i in range(1, 10, 2):
   print(i)

i = 0
while i < 10:
   print(i+1)
   i = i + 1


drinks = ["Coca Cola", "Fanta", "Sprite", "Teh Botol"]
drinks[0]

listku = ['apel', 'pisang', 'melon', 'semangka']
for i in listku:
   if i == 'pisang':
      print("Pisang ada di listku")
   else:
      print("Pisang tidak ada di listku")

def hello(name):
   print("Hello World,", name)

hello("Steven")

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