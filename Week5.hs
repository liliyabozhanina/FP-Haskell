{- Задача 1. Нормализация на входните данни

Енигма, както повечето криптиращи машини от това време, е разполагала с клавиатура със само 26-те главни 
букви от латинската азбука. Затова, преди да бъдат криптирани, всички съобщения трябвало да бъдат приведени в т. нар. 
нормален вид: всички числени стойности бивали изписвани словом, всички малки букви ставали главни, а интервалите и 
пунктуационните знакове били премахвани или заменяни с кодови комбинации от главни букви (напр. интервалът бил заменян с X и т. н.)

Напишете функция normalize message, която нормализира входното съобщение.

Правилата за нормализация са следните:

    Всички малки букви стават главни. 
    Ако съобщението съдържа цифри, функцията връща грешка. 
    Всички останали знакове се игнорират.

Примери:

normalize "Attack London tomorrow at ten a.m." = "ATTACKLONDONTOMORROWATTENAM"

normalize "Attack London tomorrow at 10 a.m." = error “digits not allowed”


Задача 2. Цезаров шифър

Цезаровият шифър е един от най-простите и най-стари методи за криптиране на съобщения. Първото му известно използване 
е от Юлий Цезар по време на кампаниите му в Галия, откъдето идва и неговото име. Идеята на Цезаровия шифър е проста: вземаме 
съобщението, което искаме да шифроваме, и заместваме всяка от буквите в него с буквата, отместена с определен брой позиции в азбуката. 
Например, ако отместването е 3, то тогава ‘A’ -> ‘D’, ‘B’ -> ‘E’, ‘C’ -> ‘F,’ … , ‘X’ -> ‘A’, ‘Y’ -> ‘B’, ‘Z’ -> ‘C’.

а) Напишете функция encode alphabet ch offset, която приема списък от знакове alphabet, знак ch и отместване offset и връща 
знака от alphabet, отместен на offset от ch (по модул дължината на списъкa). Функцията encode трябва да работи както с положително, 
така и с отрицателно отместване и да връща грешка, ако ch не е елемент на alphabet. 

N.B. Не е задължително буквите в alphabet да са подредени от ‘A’ до ‘Z’, т.е. НЕ може да разчитате на функциите ord и chr!

Примери:

encode ['A'..'Z'] 'A' 1 = 'B'

encode ['A'..'Z'] 'C' 2 = 'E'

encode ['A'..'Z'] 'Z' 3 = 'C'

encode ['A'..'Z'] 'A' (-1) = 'Z'

encode ['A'..'Z'] 'C' (-2) = 'A'

encode ['A'..'Z'] 'Z' (-3) = 'W'

encode ['A'..'Z'] '@' 1 = error “unsupported symbol: @”

б) Напишете функция encrypt alphabet offset normalized, която приема азбука alphabet, отместване offset и съобщение в нормализиран 
вид и връща съобщението, криптирано със съответното отместване.

Пример: encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM" = "FYYFHPQTSITSYTRTWWTBFYYJSFR" 

в) Напишете функция decrypt alphabet offset encrypted, която приема отместване offset и съобщение, криптирано с това отместване, 
и връща оригиналното съобщение в нормализиран вид. Можете да използвате факта, че декриптирането на Цезаров шифър с дадено 
отместване offset е еквивалентно на криптиране с отместване -offset.

Пример:

decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR" = "ATTACKLONDONTOMORROWATTENAM" -}